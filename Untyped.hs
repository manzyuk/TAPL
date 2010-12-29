{-# LANGUAGE FlexibleInstances #-}
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

type Identifier = String

data Term = Var Identifier
          | Abs Identifier Term
          | App Term Term
            deriving (Show, Eq)

-- Lexer

type Token = (SourcePos, String)

scan :: String -> Either ParseError [Token]
scan = parse toks "(stdin)"

tok  = liftA2 (,) getPosition (many1 letter
                               <|> string "("
                               <|> string ")"
                               <|> string "."
                               <|> string "\\")

toks = spaces *> many (tok <* spaces) <* eof

-- Parser

sat :: (String -> Bool) -> GenParser Token () String
sat p = token showToken posToken testToken
    where
      showToken (pos, tok) = show tok
      posToken  (pos, tok) = pos
      testToken (pos, tok) = if p tok then Just tok else Nothing

symb :: String -> GenParser Token () String
symb sym = sat (== sym) <?> show sym

identifier :: GenParser Token () String
identifier = sat (all isAlpha) <?> "identifier"

paren = between (symb "(") (symb ")")

p_var = Var <$> identifier
p_abs = liftA2 Abs (symb "\\" *> identifier <* symb ".") p_term
p_app = foldl1 App <$> many2 (p_var <|> paren p_term)
    where many2 p = liftA2 (:) p (many1 p)

p_term = choice [ try p_app
                , p_abs
                , p_var
                ] <|> paren p_term

p_program = p_term <* eof

-- Evaluator

freeVars :: Term -> Set.Set Identifier
freeVars (Var v)     = Set.singleton v
freeVars (Abs v e)   = Set.delete v (freeVars e)
freeVars (App e1 e2) = (freeVars e1) `Set.union` (freeVars e2)

type Substitution = Identifier -> Term

extend :: Substitution -> Identifier -> Term -> Substitution
extend s v t = \u -> if u == v then t else s u

-- Return the successor of a given identifier (in lexicographical order).
successor :: Identifier -> Identifier
successor = reverse . successor' . reverse
    where successor' [] = "A"
          successor' (c:cs) | c < 'z'   = (succ c) : cs
                            | otherwise = 'A' : successor cs

-- An infinite lexicographically ordered list of identifiers.
identifiers :: [Identifier]
identifiers = iterate successor "A"

freshVar :: Identifier -> Term -> Substitution -> Identifier
freshVar v e s
    = if v `Set.member` usedVars
      then head $ dropWhile (`Set.member` usedVars) identifiers
      else v
    where usedVars = Set.unions [ freeVars (s v) |
                                  v <- Set.toList . Set.delete v . freeVars $ e]

substitute :: Term -> Substitution -> Term
substitute (Var v) s     = s v
substitute (Abs v e) s   = Abs v' (substitute e s')
    where v' = freshVar v e s
          s' = extend s v (Var v')
substitute (App e1 e2) s = App (substitute e1 s) (substitute e2 s)

evalOne :: Term -> Maybe Term
evalOne (App (Abs v e) e') = Just $ substitute e (extend Var v e')
evalOne (App e e')         = (`App` e') <$> evalOne e
evalOne _                  = Nothing

eval :: Term -> Term
eval t = fromMaybe t (eval <$> evalOne t)

ppTerm :: Term -> String
ppTerm (Var v)    = v
ppTerm (Abs v e)  = "\\" ++ v ++ " . " ++ ppTerm e
ppTerm (App e e') = (pp e) ++ " " ++ (pp e')
    where pp t@(Var v)    =          ppTerm t
          pp t@(Abs v e)  = parens $ ppTerm t
          pp t@(App e e') = parens $ ppTerm t
          parens text = "(" ++ text ++ ")"

-- Interpreter

main = do
  putStr "> "
  input <- getLine
  case scan input of
    Left  err  -> putStrLn "Parse error:" >> print err
    Right toks -> case toks of
                    []      -> return ()
                    (tok:_) -> case parse ((setPosition . fst $ tok) >> p_program) "" toks of
                                 Left  err  -> putStrLn "Parse error:" >> print err
                                 Right term -> putStrLn . ppTerm $ eval term
  main