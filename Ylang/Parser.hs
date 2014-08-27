module Ylang.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Control.Applicative ((<$>), (<*>), (*>), (<*))

import Ylang.Lexer as L
import qualified Ylang.Syntax as S

import Ylang.Parser.Atomic

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
expr = Ex.buildExpressionParser [] term

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
-- >>> parse declare "<stdin>" "(: x Int)"
-- Right (: x Int)
-- >>> parse declare "<stdin>" "(: (add Int Int) Int)"
-- Right (: (add Int Int) Int)
-- >>> parse declare "<stdin>" "(: add (-> Int Int Int))"
-- Right (: (add Int Int) Int)
-- >>> parse declare "<stdin>" "(: add (-> (-> Int Int) Int))"
-- Right (: (add Int Int) Int)
declare :: Parser S.Expr
declare = L.parens form
  <?> "Declaration Expression"
  where
  form = do
    L.reservedOp ":"
    (f:args) <- example <|> simple
    ret      <- try arrow <|> variable
    return $ normalize f args ret
  example = L.parens $ many1 variable
  simple  = (:[]) <$> variable

  normalize f args ret = case ret of
    S.Arrow i args' ret'
      -> S.Declare f (args ++ (i:args')) ret'
    _
      -> S.Declare f args ret

arrow :: Parser S.Expr
arrow
  = L.parens $ do
    L.reservedOp "->"
    ts <- many1 (try arrow <|> variable)
    return $ normalize [] ts
  where
  normalize rs ts = case ts of
    []
      -> let r       = head rs
             (i:rs') = reverse $ tail rs
         in S.Arrow i rs' r
    (t:ts')
      -> case t of
        S.Arrow i as r
          -> normalize rs $ (i : as) ++ (r : ts')
        _
          -> normalize (t:rs) ts'

-- |
-- Parse Definition Syntax
-- >>> parse define "<stdin>" "(= x 10)"
-- Right (= x 10)
-- >>> parse define "<stdin>" "(= seq (-> (x y) y))"
-- Right (= (seq x y) y)
-- >>> parse define "<stdin>" "(= add (-> (x y) (+ x y)))"
-- Right (= (add x y) (+ x y))
-- >>> parse define "<stdin>" "(= (f x y) y)"
-- Right (= (f x y) y)
-- >>> parse define "<stdin>" "(= (f x y) (+ x y))"
-- Right (= (f x y) (+ x y))
define :: Parser S.Expr
define
   = L.parens form
  <?> "Definition Expression"
  where
  form = do
    L.reservedOp "="
    (f:args) <- example <|> simple
    body <- expr
    return $ normalize f args body
  example = L.parens $ many1 variable
  simple = (:[]) <$> variable
  normalize f args body = case (args, body) of
    ([], (S.Lambda i args' body'))
      -> S.Define f (i:args') body'
    _
      -> S.Define f args body

-- |
-- Parse Closure Syntax
-- >>> parse closure "<stdin>" "(-> (x) x)"
-- Right (-> x x)
-- >>> parse closure "<stdin>" "(-> (x) [x])"
-- Right (-> x [x])
closure :: Parser S.Expr
closure = L.parens form
  <?> "(-> ({ARGS}) {BODY})"
  where
  form = do
    L.reservedOp "->"
    (p:ps) <- (try $ L.parens $ many1 targ) <|> ((:[]) <$> targ)
    r      <- expr
    return $ S.Lambda p ps r

targ :: Parser S.Expr
targ
   =  try variable
  <|> try list
  <?> "Type Expression"

-- |
-- Parse Function Call (f x y z ...)
-- >>> parse call "<stdin>" "(+)"
-- Right (+)
-- >>> parse call "<stdin>" "(+ 1 2 3)"
-- Right (+ 1 2 3)
-- >>> parse call "<stdin>" "(+ x y z)"
-- Right (+ x y z)
call :: Parser S.Expr
call = L.parens form
  <?> "({CALL_FUNCTION} [{ARGS}])"
  where
  form = S.Call <$> caller <*> args
  args = (try $ many expr) <|> return []

caller :: Parser S.Expr
caller
   =  try variable
  <|> try operator
  <|> closure
  <?> "Callable funcion"

collection :: Parser S.Expr
collection
   =  pair
  <|> list -- <|> vector <|> weakmap
  <?> "Collection"

-- |
-- Parse pair literal [(, 1 2), (, yes 2), (, x no), ...]
pair :: Parser S.Expr
pair
   =  L.parens form
  <?> "Pair Expression : (, {EXPR} {EXPR})"
  where
  form = do
    L.reservedOp ","
    es <- many (many space >> expr)
    return $ modify [] es
  modify rs ts = case ts of
    []     -> S.Atom ","
    [S.Array []] -> case rs of
      []   -> S.Lambda (S.Atom "x") [] $ S.Pair (S.Array []) $ S.Atom "x"
      [S.Array []] -> S.Array []
      [s]  -> S.Array [s]
      _    -> S.Array $ reverse rs
    (l:[]) -> case rs of
      []  -> S.Lambda (S.Atom "x") [] $ S.Pair l $ S.Atom "x"
      [s] -> S.Pair s l
      _   -> S.Pair (S.Array $ reverse rs) l
    (x:xs) -> modify (x:rs) xs

-- |
-- Parse list literal [[], [1 2 3 4], [x y z] ...]
-- >>> parse list "<stdin>" "[]"
-- Right []
-- >>> parse list "<stdin>" "[1 2 3]"
-- Right [1 2 3]
-- >>> parse list "<stdin>" "[x y z]"
-- Right [x y z]
-- >>> parse list "<stdin>" "[xyz (seq y)]"
-- Right [xyz (seq y)]
list :: Parser S.Expr
list = brackets $ S.Array <$> many expr
