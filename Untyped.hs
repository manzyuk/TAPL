import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Control.Monad (forever)
import System.IO

type Name = String

data Term = Var Name
          | Abs Name Term
          | App Term Term
            deriving (Show, Eq)

-- Lexer

type Token = (SourcePos, String)

scan :: String -> Either ParseError [Token]
scan = parse toks "(stdin)"

tok  = liftA2 (,) getPosition $ choice [ many1 letter
                                       , string "("
                                       , string ")"
                                       , string "."
                                       , string "\\"
                                       ]

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

name :: GenParser Token () String
name = sat (all isAlpha) <?> "variable"

paren = between (symb "(") (symb ")")

p_var = Var <$> name
p_abs = liftA2 Abs (symb "\\" *> name <* symb ".") p_term
p_app = foldl1 App <$> many2 (p_var <|> paren p_term)
    where many2 p = liftA2 (:) p (many1 p)

p_term = choice [ try p_app
                , p_abs
                , p_var
                ] <|> paren p_term

p_program = p_term <* eof

-- Evaluator

freeVars :: Term -> Set.Set Name
freeVars (Var v)     = Set.singleton v
freeVars (Abs v e)   = Set.delete v (freeVars e)
freeVars (App e1 e2) = (freeVars e1) `Set.union` (freeVars e2)

type Substitution = Name -> Term

extend :: Substitution -> Name -> Term -> Substitution
extend s v t u = if u == v then t else s u

-- Return the successor of a given name (in lexicographical order).
successor :: Name -> Name
successor = reverse . successor' . reverse
    where successor' [] = "A"
          successor' (c:cs) | c < 'z'   = succ c : cs
                            | otherwise = 'A' : successor cs

-- An infinite lexicographically ordered list of names.
names :: [Name]
names = iterate successor "A"

freshName :: Name -> Set.Set Name -> Name
freshName n s | n `Set.member` s = head $ dropWhile (`Set.member` s) names
              | otherwise        = n

substitute :: Term -> Substitution -> Term
substitute (Var v) s     = s v
substitute (Abs v e) s   = Abs v' (substitute e s')
    where vs = Set.unions [ freeVars (s w) |
                            w <- Set.toList . Set.delete v . freeVars $ e]
          v' = freshName v vs
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
                          (tok:_) -> case parse ((setPosition . fst $ tok) >> p_program) "" toks of
                                       Left  err  -> putStrLn "Parse error:" >> print err
                                       Right term -> putStrLn . ppTerm $ eval term
