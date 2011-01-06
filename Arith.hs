import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Maybe (fromMaybe)
import Control.Monad (forever)
import System.IO

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
            deriving (Eq, Show)

-- Lexer

type Token = (SourcePos, String)

-- Split the source string into a list of tokens stripping all the whitespace.
scan :: String -> Either ParseError [Token]
scan = parse toks "(stdin)"

-- A token is either a non-empty sequence of alphanumeric chars or one of the
-- parens (,), paired with its position in the source string.
tok  = liftA2 (,) getPosition (many1 alphaNum <|> string "(" <|> string ")")
toks = spaces *> many (tok <* spaces) <* eof

-- Parser

-- Analogous to 'char', but for streams of Tokens instead of streams of chars.
symb :: String -> GenParser Token () String
symb sym = (token showToken posToken testToken) <?> show sym
    where
      showToken (pos, tok) = show tok
      posToken  (pos, tok) = pos
      testToken (pos, tok) = if tok == sym then Just tok else Nothing

-- Constants
p_zero  = TZero  <$ symb "0"
p_true  = TTrue  <$ symb "true"
p_false = TFalse <$ symb "false"

-- Return a parser that parses a symbol key followed by a term and returns
-- the parsed term.
keyword key = symb key >> p_term

-- Statements
p_if     = liftA3 TIf (keyword "if") (keyword "then") (keyword "else")
p_succ   = TSucc   <$> keyword "succ"
p_pred   = TPred   <$> keyword "pred"
p_iszero = TIsZero <$> keyword "iszero"

-- A term is either one of "if", "succ", "pred", "iszero", "true", "false",
-- "0" constructs, or it is a term enclosed in parenthesis.
p_term = choice [ p_iszero
                , p_false
                , p_true
                , p_zero
                , p_succ
                , p_pred
                , p_if
                ] <|> between (symb "(") (symb ")") p_term

-- A program is a term followed by EOF.
p_program = p_term <* eof

-- Evaluator

-- Check whether a term is a numeric value.
isNumericVal :: Term -> Bool
isNumericVal TZero     = True
isNumericVal (TSucc t) = isNumericVal t
isNumericVal _         = False

-- Check whether a term is a value.
isVal :: Term -> Bool
isVal TTrue              = True
isVal TFalse             = True
isVal t | isNumericVal t = True
isVal _                  = False

-- If some single-step evaluation rule applies to a term, do the reduction
-- and return Just the result, otherwise return Nothing.
evalOne :: Term -> Maybe Term
evalOne (TIf TTrue  c _)                     = Just c
evalOne (TIf TFalse _ a)                     = Just a
evalOne (TIf t c a)                          = (\t' -> TIf t' c a) <$> evalOne t
evalOne (TSucc t)                            = TSucc <$> evalOne t
evalOne (TPred TZero)                        = Just TZero
evalOne (TPred (TSucc t)) | isNumericVal t   = Just t
evalOne (TPred t)                            = TPred <$> evalOne t
evalOne (TIsZero TZero)                      = Just TTrue
evalOne (TIsZero (TSucc t)) | isNumericVal t = Just TFalse
evalOne (TIsZero t)                          = TIsZero <$> evalOne t
evalOne _                                    = Nothing

-- Apply single-step evaluator while there are applicable evaluation rules,
-- and return the result.
eval :: Term -> Term
eval t = fromMaybe t (eval <$> evalOne t)

-- Convert a numeric value into an integer and return Just that integer, or
-- return Nothing otherwise.
toInt :: Term -> Maybe Int
toInt TZero                      = Just 0
toInt (TSucc t) | isNumericVal t = succ <$> toInt t
toInt _                          = Nothing

-- Pretty-print a term.  Replace "numeric values" with their actual values.
ppTerm t = case toInt t of
             Just n  -> show  n
             Nothing -> show' t
    where
      show' TZero       = "0"
      show' TTrue       = "true"
      show' TFalse      = "false"
      show' (TSucc t)   = "succ " ++ show' t
      show' (TPred t)   = "pred " ++ show' t
      show' (TIsZero t) = "iszero " ++ show' t
      show' (TIf t c a) = "if " ++ show' t ++ " then " ++ show' c ++ " else " ++ show' a

-- Interpreter

prompt :: String
prompt = "> "

main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever repl
    where
      repl = do
        putStr prompt
        input <- getLine
        case scan input of
          Left  err  -> putStrLn "Parse error:" >> print err
          Right toks -> case toks of
                          []      -> return ()
                          -- In the call to 'parse' below we need to reset the initial position,
                          -- which is (line 1, column 1), to the position of the first token (in
                          -- the input).  Otherwise if the first token causes a parse error, the
                          -- location of the error won't be reported correctly.
                          (tok:_) -> case parse ((setPosition . fst $ tok) >> p_program) "" toks of
                                       Left  err  -> putStrLn "Parse error:" >> print err
                                       Right term -> putStrLn . ppTerm $ eval term
