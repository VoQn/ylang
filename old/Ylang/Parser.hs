module Ylang.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Control.Applicative ((<$>), (*>), (<*))
import Control.Arrow

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
  <|> try call
  <|> (parens expr)
  <?> "Functional Expr"

-- |
-- >>> parse declare "<stdin>" "(: x Int)"
-- Right (Declare "x" [] [] (Atom "Int"))
-- >>> parse declare "<stdin>" "(: add (-> Int Int Int))"
-- Right (Declare "add" [] [Atom "Int",Atom "Int"] (Atom "Int"))
-- >>> parse declare "<stdin>" "(: add (-> (-> Int Int) Int))"
-- Right (Declare "add" [] [Arrow (Atom "Int") [] (Atom "Int")] (Atom "Int"))
declare :: Parser S.Expr
declare = (L.parens $ L.reserved ":" >> form)
  <?> "Declaration Expression"
  where
  form = do
    (S.Atom n) <- try variable <|> operator
    ps         <- many $ try declare
    (as, r)    <- cleaning <$> (try targ <|> arrow)
    return $ S.Declare n ps as r
  cleaning ty = case ty of
    S.Atom        _ -> ([], ty)
    S.Array       _ -> ([], ty)
    S.Pair      _ _ -> ([], ty)
    S.Call      _ _ -> ([], ty)
    S.Arrow t ts r' -> ((t:ts), r')
    _ -> undefined

arrow :: Parser S.Expr
arrow = L.parens form
  where
  form = do
    L.reservedOp "->"
    f <- typeExpr
    s <- typeExpr
    r <- many typeExpr
    return $ modify f s r
  typeExpr = try arrow <|> targ
  modify f s [] = S.Arrow f [] s
  modify f s rs =
    let (r',rs') = (last &&& init) rs
    in S.Arrow f (s:rs') r'

-- |
-- Parse Definition Syntax
-- >>> parse define "<stdin>" "(= x 10)"
-- Right (Define "x" (Int 10))
-- >>> parse define "<stdin>" "(= seq (\\ (x y) y))"
-- Right (Define "seq" (Func (Atom "x") [Atom "y"] [] (Atom "y")))
-- >>> parse define "<stdin>" "(= add (\\ (x y) (+ x y)))"
-- Right (Define "add" (Func (Atom "x") [Atom "y"] [] (Call (Atom "+") [Atom "x",Atom "y"])))
-- >>> parse define "<stdin>" "(= (f x y) y)"
-- Right (Define "f" (Func (Atom "x") [Atom "y"] [] (Atom "y")))
-- >>> parse define "<stdin>" "(= (f x y) (+ x y))"
-- Right (Define "f" (Func (Atom "x") [Atom "y"] [] (Call (Atom "+") [Atom "x",Atom "y"])))
define :: Parser S.Expr
define = L.parens form
  <?> "Definition Expression"
  where
  form = do
    L.reservedOp "="
    (v,e) <- lambdaExpr <|> exampleExpr
    return $ S.Define (getName v) e
  getName (S.Atom n) = n
  getName _ = undefined

lambdaExpr :: Parser (S.Expr, S.Expr)
lambdaExpr = do
  i <- (try variable <|> operator)
  f <- (try closure <|> expr)
  return (i, f)

exampleExpr :: Parser (S.Expr, S.Expr)
exampleExpr = do
  (f:a:as) <- L.parens $ many targ
  (es,r)   <- pop <$> many expr
  return (f, S.Func a as es r)

pop :: [a] -> ([a], a)
pop [] = undefined
pop xs = (init &&& last) xs

-- |
-- Parse Closure Syntax
-- >>> parse closure "<stdin>" "(\\ (x) x)"
-- Right (Func (Atom "x") [] [] (Atom "x"))
-- >>> parse closure "<stdin>" "(\\ (x) [x])"
-- Right (Func (Atom "x") [] [] (Array [Atom "x"]))
closure :: Parser S.Expr
closure = L.parens form
  <?> "(-> ({ARGS}) {BODY})"
  where
  form = do
    L.reservedOp "\\"
    (a:as) <- try manyArgs <|> unitArg
    (es,r) <- pop <$> many expr
    return $ S.Func a as es r
  manyArgs = L.parens $ many targ
  unitArg  = (:[]) <$> targ

targ :: Parser S.Expr
targ
   =  try pair
  <|> try list
  <|> variable
  <?> "Type Expression"

-- |
-- Parse Function Call (f x y z ...)
-- >>> parse call "<stdin>" "(+)"
-- Right (Atom "+")
-- >>> parse call "<stdin>" "(+ 1 2 3)"
-- Right (Call (Atom "+") [Int 1,Int 2,Int 3])
-- >>> parse call "<stdin>" "(+ x y z)"
-- Right (Call (Atom "+") [Atom "x",Atom "y",Atom "z"])
call :: Parser S.Expr
call = L.parens form
  <?> "({CALL_FUNCTION} [{ARGS}])"
  where
  form = do
    func <- caller
    args <- many (many space >> expr)
    return $ modify func args
  modify f [] = f
  modify f as = S.Call f as

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
pair = L.parens form
  <?> "Pair Expression : (, {EXPR} {EXPR})"
  where
  form = do
    L.reservedOp ","
    es <- many (many space >> expr)
    return $ modify [] es
  modify rs ts = case ts of
    []     -> S.Atom ","
    [S.Array []] -> case rs of
      []   -> S.Func (S.Atom "x") [] [] (S.Pair (S.Array []) (S.Atom "x"))
      [S.Array []] -> S.Array [S.Array []]
      [s]  -> S.Array [s]
      _    -> S.Array $ reverse rs
    (l:[]) -> case rs of
      []  -> S.Func (S.Atom "x") [] [] (S.Pair l (S.Atom "x"))
      [s] -> S.Pair s l
      _   -> S.Pair (S.Array $ reverse rs) l
    (x:xs) -> modify (x:rs) xs

-- |
-- Parse list literal [[], [1 2 3 4], [x y z] ...]
-- >>> parse list "<stdin>" "[]"
-- Right (Array [])
-- >>> parse list "<stdin>" "[1 2 3]"
-- Right (Array [Int 1,Int 2,Int 3])
-- >>> parse list "<stdin>" "[x y z]"
-- Right (Array [Atom "x",Atom "y",Atom "z"])
-- >>> parse list "<stdin>" "[xyz (seq y)]"
-- Right (Array [Atom "xyz",Call (Atom "seq") [Atom "y"]])
list :: Parser S.Expr
list = brackets $ S.Array <$> many expr
