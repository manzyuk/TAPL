import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Maybe (fromMaybe)

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
            deriving (Eq, Show)

-- Parser

-- Return a parser that parses a literal constant matching pat and returns val.
constant pat val = string pat >> return val

p_zero  = constant "0"     TZero
p_true  = constant "true"  TTrue
p_false = constant "false" TFalse

-- Return a parser that parses a keyword matching pat followed by a space and a
-- term, and returns the parsed term.
keyword pat = string pat >> space *> p_term

p_if     = liftA3 TIf (keyword "if") (keyword "then") (keyword "else")
p_succ   = TSucc   <$> keyword "succ"
p_pred   = TPred   <$> keyword "pred"
p_iszero = TIsZero <$> keyword "iszero"

-- A term can be preceeded and followed by spaces.  A term with all
-- spaces stripped is either one of "if", "succ", "pred", "iszero",
-- "true", "false", "0" constructs, or it is a term enclosed in
-- parenthesis.
p_term = spaces *> (p_term' <|> parens p_term) <* spaces
    where p_term' = (choice $ map try
                     [ p_if
                     , p_true
                     , p_zero
                     , p_succ
                     , p_pred
                     , p_false
                     , p_iszero
                     ])
          parens  = between (char '(') (char ')')

-- A program is a term followed by EOF.
p_program = p_term <* eof

-- Evaluator

-- Check whether a term is a numeric value.
isNumericVal :: Term -> Bool
isNumericVal TZero     = True
isNumericVal (TSucc t) = isNumericVal t

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

-- Convert a numeric value into an integer, and return Just that integer or
-- Nothing otherwise.
toInt :: Term -> Maybe Int
toInt TZero                      = Just 0
toInt (TSucc t) | isNumericVal t = succ <$> toInt t
toInt _                          = Nothing

-- Pretty-print a term.  If it is a numeric value replace it with its value.
ppTerm t = case toInt t of
             Just n  -> show  n
             Nothing -> show' t
    where
      show' TZero       = "0"
      show' TTrue       = "true"
      show' TFalse      = "false"
      show' (TSucc t)   = "succ " ++ show t
      show' (TPred t)   = "pred " ++ show t
      show' (TIsZero t) = "iszero " ++ show t
      show' (TIf t c a) = "if " ++ show t ++ " then " ++ show c ++ " else " ++ show a

-- Interpreter

main = do
  putStr "> "
  input <- getLine
  case parse p_program "(stdin)" input of
    Left err -> print err
    Right  t -> putStrLn . ppTerm $ eval t
  main