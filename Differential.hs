{-# LANGUAGE ScopedTypeVariables #-}

-- Toy implementation of an evaluator for Ehrhard-Regnier's differential
-- lambda-calculus.  See
--
--   Thomas Ehrhard, Laurent Regnier, The differential lambda-calculus,
--   Theoretical Computer Science 309 (2003) 1-41.
--
-- for more details and definitions.

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Text.ParserCombinators.Parsec.Token    as T
import qualified Text.ParserCombinators.Parsec.Language as L
import Control.Applicative
import Data.List (intercalate)

import Control.Monad (forever)
import System.IO

-- For experimental stuff only.
import System.IO.Unsafe
import qualified Control.Exception as E


type Name = String

-- The ground ring.  The type Term should be really parametrized over
-- the types that are instances of Num, but for our exploratory
-- purposes taking numbers to be Double is enough.
type Ring = Double

-- Primitive functions.
data Func = Exp
          | Sin
          | Cos
            deriving (Show, Eq)

app :: Func -> Ring -> Ring
app Exp = exp
app Sin = sin
app Cos = cos

-- Instead of making the set of terms into a module over the numbers,
-- let us add numbers as first-class objects to the language.
data Term = Var Name
          | Num Ring
          | Pri Func
          | Abs Name Term
          | App Term Term
          | Dif Term Term
          | Add Term Term
          | Mul Term Term
            deriving (Show, Eq)

-- One step reduction: if some reduction rule applies to a term, do
-- the reduction and return Just the result, otherwise return Nothing.
evalOne :: Term -> Maybe Term

-- linearity of lambda
evalOne (Abs v (Add s1 s2))       = Just $ Add (Abs v s1) (Abs v s2)
evalOne (Abs v (Mul r@(Num _) s)) = Just $ Mul r (Abs v s)
evalOne (Abs v (Mul s r@(Num _))) = Just $ Mul (Abs v s) r

-- beta-reduction
evalOne (App (Abs v s) t)         = Just $ substitute s (extend Var v t)

-- application of primitive functions
evalOne (App (Pri f) (Num x))     = Just . Num $ app f x

-- linearity of application with respect to the first argument
evalOne (App (Add s1 s2) t)       = Just $ Add (App s1 t) (App s2 t)
evalOne (App (Mul r@(Num _) s) t) = Just $ Mul r (App s t)
evalOne (App (Mul s r@(Num _)) t) = Just $ Mul (App s t) r

-- pun a number with a function that multiplies its argument with that number
evalOne (App (Num n) t)           = Just $ Mul (Num n) t

evalOne (App s t)                 = (`App` t) <$> evalOne s

-- reduction rule for differentiation
evalOne (Dif (Abs v s) u)         = Just $ Abs v' (partial v s' u)
    where v' = freshName v (freeVars u)
          s' = substitute s $ extend Var v (Var v')

-- differentiation of primitive functions
evalOne (Dif (Pri Exp) u)         = Just $ dif Exp u
evalOne (Dif (Pri Sin) u)         = Just $ dif Cos u
evalOne (Dif (Pri Cos) u)         = Just $ dif Sin (neg u)

-- linearity of differentiation in both arguments
evalOne (Dif (Add s1 s2) u)       = Just $ Add (Dif s1 u) (Dif s2 u)
evalOne (Dif s (Add u1 u2))       = Just $ Add (Dif s u1) (Dif s u2)
evalOne (Dif (Mul r@(Num _) s) u) = Just $ Mul r (Dif s u)
evalOne (Dif (Mul s r@(Num _)) u) = Just $ Mul (Dif s u) r
evalOne (Dif s (Mul r@(Num _) u)) = Just $ Mul r (Dif s u)
evalOne (Dif s (Mul u r@(Num _))) = Just $ Mul (Dif s u) r

evalOne (Dif s u)                 = (`Dif` u) <$> evalOne s

-- basic simplification rules
evalOne (Add (Num n) (Num m))     = Just $ Num (n + m)
evalOne (Add s@(Num n) t)         = (s `Add`) <$> (evalOne t)
evalOne (Add s t)                 = (`Add` t) <$> (evalOne s)
evalOne (Mul (Num 0) _)           = Just $ Num 0
evalOne (Mul _ (Num 0))           = Just $ Num 0
evalOne (Mul (Num 1) t)           = Just t
evalOne (Mul s (Num 1))           = Just s
evalOne (Mul (Num n) (Num m))     = Just $ Num (n * m)
evalOne (Mul s@(Num n) t)         = (s `Mul`) <$> (evalOne t)
evalOne (Mul s t)                 = (`Mul` t) <$> (evalOne s)

evalOne _                         = Nothing

eval :: Term -> Term
eval t = fromMaybe t (eval <$> evalOne t)

dif :: Func -> Term -> Term
dif f u = Abs x (Mul (App (Pri f) (Var x)) u)
    where x = freshName "x" (freeVars u)

neg :: Term -> Term
neg = Mul (Num (-1))

-- Example:
--
-- d            d              |    |
-- -- (\x . x * -- (\y . x * y)|   )|
-- dx           dy             |y=2 |x=1
--
-- translates into (formatted for readability)
--
-- eval $ App (Dif (Abs "x"
--                      (Mul (Var "x")
--                           (App (Dif (Abs "y"
--                                          (Mul (Var "x")
--                                               (Var "y")))
--                                     (Num 1))
--                                (Num 2))))
--                 (Num 1))
--            (Num 1)
--
-- and produces Num 2.0

-- The following is an attempt to write a proper rule-based reduction
-- system.  A reduction rule is a function of type Term -> Maybe Term
-- that returns either Just the reduced term or Nothing if the
-- reduction rule does not apply.

-- The function tryEach tries each of the supplied rules until one of
-- them returns Just something or Nothing if no rule applies.
tryEach :: [a -> Maybe a] -> a -> Maybe a
tryEach fs x = foldr ((<|>) . ($ x)) Nothing fs

-- The following is a quick and dirty hack: I want to write reduction
-- rules as lambdas, but I don't want to explicitly handle the case
-- when the pattern does not match.
trap :: (a -> Maybe a) -> a -> Maybe a
trap f = unsafePerformIO
       . E.handle (\ (_ :: E.PatternMatchFail) -> return Nothing)
       . E.evaluate
       . f

-- Using trap is subtle due to laziness (an exception may be thrown
-- at the point where no handler is available).  For example, you
-- cannot factor out Just $ into map (Just .) below.
evalOne' = tryEach . map trap $
           [ \ (Abs v (Add s1 s2))       -> Just $ Add (Abs v s1) (Abs v s2)
           , \ (Abs v (Mul r@(Num _) s)) -> Just $ Mul r (Abs v s)
           , \ (Abs v (Mul s r@(Num _))) -> Just $ Mul (Abs v s) r
           , \ (App (Abs v s) t)         -> Just $ substitute s (extend Var v t)
           , \ (App (Pri f) (Num x))     -> Just . Num $ app f x
           , \ (App (Add s1 s2) t)       -> Just $ Add (App s1 t) (App s2 t)
           , \ (App (Mul r@(Num _) s) t) -> Just $ Mul r (App s t)
           , \ (App (Mul s r@(Num _)) t) -> Just $ Mul (App s t) r
           , \ (App (Num n) t)           -> Just $ Mul (Num n) t
           , \ (Dif (Abs v s) u)         -> let v' = freshName v (freeVars u)
                                                s' = substitute s $ extend Var v (Var v')
                                            in Just $ Abs v' (partial v s' u)
           , \ (Dif (Pri Exp) u)         -> Just $ dif Exp u
           , \ (Dif (Pri Sin) u)         -> Just $ dif Cos u
           , \ (Dif (Pri Cos) u)         -> Just $ dif Sin (neg u)
           , \ (Dif (Add s1 s2) u)       -> Just $ Add (Dif s1 u) (Dif s2 u)
           , \ (Dif s (Add u1 u2))       -> Just $ Add (Dif s u1) (Dif s u2)
           , \ (Dif (Mul r@(Num _) s) u) -> Just $ Mul r (Dif s u)
           , \ (Dif (Mul s r@(Num _)) u) -> Just $ Mul (Dif s u) r
           , \ (Dif s (Mul r@(Num _) u)) -> Just $ Mul r (Dif s u)
           , \ (Dif s (Mul u r@(Num _))) -> Just $ Mul (Dif s u) r
           , \ (Add (Num n) (Num m))     -> Just $ Num (n + m)
           , \ (Mul (Num 0) _)           -> Just $ Num 0
           , \ (Mul _ (Num 0))           -> Just $ Num 0
           , \ (Mul (Num 1) t)           -> Just $ t
           , \ (Mul s (Num 1))           -> Just $ s
           , \ (Mul (Num n) (Num m))     -> Just $ Num (n * m)
           , \ (App s t)                 -> (`App` t) <$> evalOne' s
           , \ (App s t)                 -> (s `App`) <$> evalOne' t
           , \ (Dif s t)                 -> (`Dif` t) <$> evalOne' s
           , \ (Dif s t)                 -> (s `Dif`) <$> evalOne' t
           , \ (Add s t)                 -> (`Add` t) <$> evalOne' s
           , \ (Add s t)                 -> (s `Add`) <$> evalOne' t
           , \ (Mul s t)                 -> (`Mul` t) <$> evalOne' s
           , \ (Mul s t)                 -> (s `Mul`) <$> evalOne' t
           ]

eval' :: Term -> Term
eval' t = fromMaybe t (eval' <$> evalOne' t)

-- Utilities

freeVars :: Term -> Set.Set Name
freeVars (Var v)   = Set.singleton v
freeVars (Num _)   = Set.empty
freeVars (Pri _)   = Set.empty
freeVars (Abs v s) = Set.delete v (freeVars s)
freeVars (App s t) = (freeVars s) `Set.union` (freeVars t)
freeVars (Dif s t) = (freeVars s) `Set.union` (freeVars t)
freeVars (Add s t) = (freeVars s) `Set.union` (freeVars t)
freeVars (Mul s t) = (freeVars s) `Set.union` (freeVars t)

type Substitution = Name -> Term

extend :: Substitution -> Name -> Term -> Substitution
extend d v t = \u -> if u == v then t else d u

-- Return the successor of a given name (in lexicographical order).
successor :: Name -> Name
successor = reverse . successor' . reverse
    where successor' [] = "A"
          successor' (c:cs) | c < 'z'   = (succ c) : cs
                            | otherwise = 'A' : successor cs

-- An infinite lexicographically ordered list of names.
names :: [Name]
names = iterate successor "A"

freshName :: Name -> Set.Set Name -> Name
freshName n s | n `Set.member` s = head $ dropWhile (`Set.member` s) names
              | otherwise        = n

substitute :: Term -> Substitution -> Term
substitute (Var v) d     = d v
substitute t@(Num _) _   = t
substitute t@(Pri _) _   = t
substitute (Abs v e) d   = Abs v' (substitute e d')
    where vs = Set.unions [ freeVars (d w) |
                            w <- Set.toList . Set.delete v . freeVars $ e]
          v' = freshName v vs
          d' = extend d v (Var v')
substitute (App e1 e2) d = App (substitute e1 d) (substitute e2 d)
substitute (Dif e1 e2) d = Dif (substitute e1 d) (substitute e2 d)
substitute (Add e1 e2) d = Add (substitute e1 d) (substitute e2 d)
substitute (Mul e1 e2) d = Mul (substitute e1 d) (substitute e2 d)

partial :: Name -> Term -> Term -> Term
partial x (Var y) u
    | x == y    = u
    | otherwise = Num 0
partial _ (Num _) _   = Num 0
partial _ (Pri _) _   = Num 0
partial x (Abs y s) u = Abs y' (partial x s' u)
    where y' = freshName y (Set.singleton x)
          s' = substitute s $ extend Var y (Var y')
partial x (App s t) u = Add (App (partial x s u) t) (App (Dif s (partial x t u)) t)
partial x (Dif s t) u = Add (Dif (partial x s u) t) (Dif s (partial x t u))
partial x (Add s t) u = Add (partial x s u) (partial x t u)
partial x (Mul s t) u = Add (Mul (partial x s u) t) (Mul s (partial x t u))

-- Lexer

languageDef = L.emptyDef { L.reservedNames = ["+", "*", "lambda", "derive", "exp", "sin", "cos"] }

lexer = T.makeTokenParser languageDef

parens         = T.parens lexer
reserved       = T.reserved lexer
identifier     = T.identifier lexer
naturalOrFloat = T.naturalOrFloat lexer

-- Parser

p_var = Var <$> identifier
p_num = (Num . either fromInteger id) <$> naturalOrFloat
p_pri = Pri <$> (choice . map pri $ [ (Exp, "exp")
                                    , (Sin, "sin")
                                    , (Cos, "cos")
                                    ]) <?> "primitive"
    where pri (c, s) = c <$ reserved s

form c r p1 p2 = (maybe (return ()) reserved r) *> liftA2 c p1 p2

p_abs = form Abs (Just "lambda") identifier p_term
p_dif = form Dif (Just "derive") p_term     p_term
p_add = form Add (Just "+")      p_term     p_term
p_mul = form Mul (Just "*")      p_term     p_term
p_app = form App Nothing         p_term     p_term

p_term = atom <|> (parens list <?> "list")
    where atom = choice [ p_var
                        , p_num
                        , p_pri
                        ]
          list = choice [ p_abs
                        , p_app
                        , p_dif
                        , p_add
                        , p_mul
                        ]

p_program = p_term <* eof

ppTerm :: Term -> String
ppTerm (Var v)   = v
ppTerm (Num n)   = show n
ppTerm (Pri Exp) = "exp"
ppTerm (Pri Sin) = "sin"
ppTerm (Pri Cos) = "cos"
ppTerm (Abs v s) = list [ "lambda"
                        , v
                        , ppTerm s
                        ]
ppTerm (App s t) = list [ ppTerm s
                        , ppTerm t
                        ]
ppTerm (Dif s t) = list [ "derive"
                        , ppTerm s
                        , ppTerm t
                        ]
ppTerm (Add s t) = list [ "+"
                        , ppTerm s
                        , ppTerm t
                        ]
ppTerm (Mul s t) = list [ "*"
                        , ppTerm s
                        , ppTerm t
                        ]

list :: [String] -> String
list = wrap . intercalate " "
    where wrap text = "(" ++ text ++ ")"

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
        case parse p_program "(stdin)" input of
          Left  err  -> putStrLn "Parse error:" >> print err
          Right term -> putStrLn . ppTerm $ eval term
