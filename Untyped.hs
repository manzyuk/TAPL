import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Data.Char (isAlphaNum)

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

data Term = Var String
          | Abs String Term
          | App Term Term
            deriving (Show, Eq)

-- Lexer

type Token = (SourcePos, String)

scan :: String -> Either ParseError [Token]
scan = parse toks "(stdin)"

tok  = liftA2 (,) getPosition (identifier
                               <|> string "("
                               <|> string ")"
                               <|> string "."
                               <|> string "\\")
    where identifier = liftA2 (:) letter (many alphaNum)

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

variable :: GenParser Token () String
variable = sat (all isAlphaNum) <?> "variable"

paren = between (symb "(") (symb ")")

p_var = Var <$> variable
p_abs = liftA2 Abs (symb "\\" *> variable <* symb ".") p_term
p_app = foldl1 App <$> many2 (p_var <|> paren p_term)
    where many2 p = liftA2 (:) p (many1 p)

p_term = choice [ try p_app
                , p_abs
                , p_var
                ] <|> paren p_term

p_program = p_term <* eof
