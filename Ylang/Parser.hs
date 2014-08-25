module Ylang.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Control.Applicative ((<$>), (<*>), (*>), (<*))

import qualified Ylang.Lexer as L
import qualified Ylang.Syntax as S

makeStdinParser :: Parser a -> String -> Either ParseError a
makeStdinParser p s = parse (contents p) "<stdin>" s

parseExpr :: String -> Either ParseError S.Expr
parseExpr = makeStdinParser expr

parseTopLevel :: String -> Either ParseError [S.Expr]
parseTopLevel = makeStdinParser topLevel

contents :: Parser a -> Parser a
contents p = (try $ many space) *> p <* eof

topLevel :: Parser [S.Expr]
topLevel = many $ factor

expr :: Parser S.Expr
expr = Ex.buildExpressionParser table term

table = [[]]

term :: Parser S.Expr
term
   =  try factor
  <|> try atom
  <|> collection

factor :: Parser S.Expr
factor
   =  try declare
  <|> try define
  <|> try closure
  <|> call
  <?> "Functional Expr"

-- |
-- >>> parse declare "<stdin>" "(dec x Int)"
-- Right (x : Int)
--
-- >>> parse declare "<stdin>" "(dec add (-> (Int Int) Int))"
-- Right (add : ((Int -> Int) -> Int))
--
declare :: Parser S.Expr
declare = L.parens form <?> "Declaration Expression"
  where
  form   = L.reserved "dec" *> (wrap <$> identifier <*> bind)
  wrap i = uncurry $ S.Declare i
  bind   = lambda <|> ((,) []) <$> targ

-- |
-- Parse Definition Syntax
--
-- >>> parse define "<stdin>" "(def x 10)"
-- Right (x = 10)
--
-- >>> parse define "<stdin>" "(def seq (-> (x y) y))"
-- Right ((seq x y) = y)
--
-- >>> parse define "<stdin>" "(def add (-> (x y) (+ x y)))"
-- Right ((add x y) = (+ x y))
--
define :: Parser S.Expr
define = L.parens form <?> "Definition Expression"
  where
  form   = L.reserved "def" *> (wrap <$> identifier <*> bind)
  wrap i = uncurry $ S.Define i
  bind   = lambda <|> ((,) []) <$> expr

-- |
-- Parse Closure Syntax
--
-- >>> parse closure "<stdin>" "(-> x x)"
-- Right (x -> x)
--
-- >>> parse closure "<stdin>" "(-> x [x])"
-- Right (x -> [x])
--
closure :: Parser S.Expr
closure = form <?> "(-> {ARGS} {BODY})"
  where
  form = uncurry S.Lambda <$> lambda

targ :: Parser S.Expr
targ
   =  try variable
  <|> try list
  <?> "Type Expression"

-- |
-- Parse Lambda Expression
--
-- >>> parse lambda "<stdin>" "(-> x x)"
-- Right ([x],x)
--
-- >>> parse lambda "<stdin>" "(-> (x y) z)"
-- Right ([x,y],z)
--
lambda :: Parser ([S.Expr], S.Expr)
lambda = L.parens form <?> "Lambda Expression"
  where
  form = L.reservedOp "->" *> ((,) <$> args <*> expr)
  args = (L.parens $ many1 targ) <|> ((:[]) <$> targ)

-- |
-- Parse Function Call (f x y z ...)
--
-- >>> parse call "<stdin>" "(+)"
-- Right (+)
--
-- >>> parse call "<stdin>" "(+ 1 2 3)"
-- Right (+ 1 2 3)
--
-- >>> parse call "<stdin>" "(+ x y z)"
-- Right (+ x y z)
--
call :: Parser S.Expr
call = L.parens form <?> "({CALL_FUNCTION} [{ARGS}])"
  where
  form = S.Call <$> caller <*> args
  args = (try $ many expr) <|> return []

caller :: Parser S.Expr
caller
   =  try variable
  <|> try operator
  <|> closure
  <?> "Callable funcion"

-- |
-- Parse Variables
--
-- >>> parse variable "<stdin>" "x"
-- Right x
--
-- >>> parse variable "<stdin>" "empty?"
-- Right empty?
--
-- >>> parse variable "<stdin>" "add#"
-- Right add#
--
-- >>> parse variable "<stdin>" "x->string"
-- Right x->string
--
variable :: Parser S.Expr
variable = S.Var <$> identifier

-- |
-- Parse Operator
--
-- >>> parse operator "<stdin>" "+"
-- Right +
--
operator :: Parser S.Expr
operator = S.Operator <$> many1 symbol <* many space

-- |
-- Parse Symbol Identifier
--
-- >>> parse identifier "<stdin>" "x"
-- Right "x"
--
-- >>> parse identifier "<stdin>" "x->string"
-- Right "x->string"
--
identifier :: Parser String
identifier = (:) <$> istart <*> ichars <* many space
  where
  istart = letter
  ichars = many $ alphaNum <|> symbol

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

atom :: Parser S.Expr
atom
   =  try float
  <|> try int
  <|> try bool
  <|> try str
  <|> try operator
  <|> variable
  <?> "Atomic Value"

collection :: Parser S.Expr
collection
   = list
-- <|> vector
-- <|> weakmap
  <?> "Collection"

-- |
-- Parse list literal [[], [1 2 3 4], [x y z] ...]
--
-- >>> parse list "<stdin>" "[]"
-- Right []
--
-- >>> parse list "<stdin>" "[1 2 3]"
-- Right [1 2 3]
--
-- >>> parse list "<stdin>" "[x y z]"
-- Right [x y z]
--
-- >>> parse list "<stdin>" "[xyz (seq y)]"
-- Right [xyz (seq y)]
--
list :: Parser S.Expr
list = L.brackets $ S.List <$> many expr

-- |
-- Parse integer number [...-2 -1, 0, 1 2 ...]
--
-- >>> parse int "<stdin>" "0"
-- Right 0
--
-- >>> parse int "<stdin>" "-2"
-- Right -2
--
-- >>> parse int "<stdin>" "0xff"
-- Right 255
--
int :: Parser S.Expr
int = S.Int <$> L.integer

-- |
-- Parse flaoting point number [... -1.0 -0.5 0.0 0.5 1.0 ...]
--
-- >>> parse float "<stdin>" "0.0"
-- Right 0.0
--
-- >>> parse float "<stdin>" "0.5"
-- Right 0.5
--
-- >>> parse float "<stdin>" "-0.5"
-- Right -0.5
--
float :: Parser S.Expr
float = wrap <$> signed <*> L.float
  where
  wrap s v = S.Float $ s v
  signed   = try minus <|> return id
  minus    = char '-' >> return ((-) 0)

-- |
-- Parse boolean identifier [Yes/No]
--
-- >>> parse bool "<stdin>" "#t"
-- Right #t
--
-- >>> parse bool "<stdin>" "#f"
-- Right #f
--
bool :: Parser S.Expr
bool = S.Boolean <$> ("#t" ?> True <|> "#f" ?> False)
  where
  k ?> r = try (string k) >> return r

-- |
-- Parse String literal
--
-- >>> parse str "<stdin>" "\"\""
-- Right ""
--
-- >>> parse str "<stdin>" "\"Hello!\""
-- Right "Hello!"
--
str :: Parser S.Expr
str = S.String <$> L.strings
