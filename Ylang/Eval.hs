module Ylang.Eval where

import Data.List as List (intercalate)
import Data.Set ((\\), Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Ylang.Syntax

-- |
-- Show code typing by User
--
-- >>> yexpr $ Int 1
-- "1"
--
-- >>> yexpr $ Float 0.5
-- "0.5"
--
-- >>> yexpr $ List [Int 1,Int 2,Int 3]
-- "[1 2 3]"
--
-- >>> yexpr $ Lambda [Var "x"] (Var "x")
-- "(-> (x) x)"
--
-- >>> yexpr $ Call (Operator "+") [Var "x",Var "y",Int 10]
-- "(+ x y 10)"
--
yexpr :: Expr -> String
yexpr expr = case expr of
  -- Atomic Value
  Var      v -> v
  Boolean  b -> if b then "Yes" else "No"
  Int      n -> show n
  Float    f -> show f
  String   s -> s
  Operator p -> p

  -- Collection
  List es
    -> '[' : (yexpr' es) ++ "]"

  -- Function
  Lambda xs b
    -> "(-> (" ++ (yexpr' xs) ++ ") " ++ (yexpr b) ++ ")"

  Call f args
    -> '(' : (yexpr f) ++ " " ++ (yexpr' args) ++ ")"

  -- Otherwise
  _ -> show expr

  where
  yexpr' = intercalate " " . map yexpr

-- |
-- Find Free Variables from Lambda Expression
--
-- (-> x x) ... []
-- >>> freeVars $ Lambda [Var "x"] (Var "x")
-- fromList []
--
-- (-> x y) ... [y]
-- >>> freeVars $ Lambda [Var "x"] (Var "y")
-- fromList [Var "y"]
--
-- (-> (x y) [w,x,y,z]) ... [w,z]
-- >>> let args = [Var "x",Var "y"]
-- >>> let expr = List [Var "w",Var "x",Var "y",Var "z"]
-- >>> freeVars $ Lambda args expr
-- fromList [Var "w",Var "z"]
--
-- (-> x (+ x 1))
-- >>> let args = [Var "x"]
-- >>> let expr = Call (Operator "+") [Var "x",Int 1]
-- >>> freeVars $ Lambda args expr
-- fromList []
--
-- (-> x (+ y 1))
-- >>> let args = [Var "x"]
-- >>> let expr = Call (Operator "+") [Var "y",Int 1]
-- >>> freeVars $ Lambda args expr
-- fromList [Var "y"]
--
-- ((-> y y) 1) ... []
-- >>> let func = Lambda [Var "y"] $ Var "y"
-- >>> freeVars $ Call func [Int 1]
-- fromList []
--
-- ((-> y x) 1) ... [x]
-- >>> let func = Lambda [Var "y"] $ Var "x"
-- >>> freeVars $ Call func [Int 1]
-- fromList [Var "x"]
--
freeVars :: Expr -> Set Expr
freeVars expr = case expr of
  v@(Var _)
    -> Set.singleton v

  List es
    -> collect es

  Define name args expr'
    -> (freeVars expr') \\ (Set.insert (Var name) $ collect args)

  Lambda args expr'
    -> (freeVars expr') \\ (collect args)

  Call f args -> case f of
    g@(Lambda _ _)
      -> Set.union (freeVars g) $ collect args

    Operator _
      -> collect args

    v@(Var _)
      -> Set.insert v $ collect args

  _ -- expression has not closed scope
    -> Set.empty

  where
  collect = foldr (Set.union . freeVars) Set.empty

-- |
-- Alpha Conversion for Lambda Calculus
--
-- Convertable case:
-- (-> x ((-> x x) x)) ... (-> y ((-> x x) y))
-- >>> let g = Lambda [Var "x"] $ Var "x"
-- >>> let f = Lambda [Var "x"] $ Call g [Var "x"]
-- >>> alpha $ f
-- Lambda [Var "y_0"] (Call (Lambda [Var "x_0"] (Var "x_0")) [Var "y_0"])
--
-- Not-Convertable case:
-- (-> x ((-> y x) x)) ... (-> x ((-> y x) x))
-- >>> let g = Lambda [Var "y"] $ Var "x"
-- >>> let f = Lambda [Var "x"] $ Call g [Var "x"]
-- >>> alpha $ f
-- Lambda [Var "x"] (Call (Lambda [Var "y"] (Var "x")) [Var "x"])
--
alpha :: Expr -> Expr
alpha expr = case expr of
  f@(Lambda ys (Call g@(Lambda xs ex) zs))
    | hasNotOutScopeBind g ->
        let
          xs' = rename "x_" 0 [] xs
          ys' = rename "y_" 0 [] ys
          ex' = apply (Map.fromList $ zip xs xs') ex
        in Lambda ys' $ Call (Lambda xs' ex') ys'
    | otherwise -> f
  _
    -> expr
  where
  hasNotOutScopeBind g = Set.null $ freeVars g

builtins :: Map Expr Expr
builtins = Map.fromList
  [
      (Var "id", Lambda [Var "x"] (Var "x"))
    , (Var "seq", Lambda [Var "x", Var "y"] (Var "y"))
  ]

-- |
-- Evaluate Expression
--
-- >>> let env = Map.empty
-- >>> fst $ eval env (Var "x")
-- Var "x"
--
-- >>> let env  = Map.empty
-- >>> let func = Lambda [Var "x"] (Var "x")
-- >>> let expr = Call func [Var "y"]
-- >>> fst $ eval Map.empty expr
-- Var "y"
--
-- >>> let env = Map.fromList [(Var "x",Int 100)]
-- >>> fst $ eval env (Var "x")
-- Int 100
--
eval :: Map Expr Expr -> Expr -> (Expr, Map Expr Expr)
eval env (Call f args) = eval env $ applyf f args
eval env expr =
  let expr' = maybe expr id $ Map.lookup expr env
  in (expr', env)

-- |
-- Apply Function with Arguments
--
-- id : (-> x x)
-- >>> applyf (Lambda [Var "x"] (Var "x")) [Int 10]
-- Int 10
--
applyf :: Expr -> [Expr] -> Expr
applyf (Lambda (x@(Var _):[]) y@(Var _)) args@(a:as)
  | x == y      = a
  | elem y args = y
  | otherwise   = List []

-- |
-- Rename Variable identifier
--
-- >>> let vars = [Var "foo",Var "bar"]
-- >>> rename "x_" 0 [] vars
-- [Var "x_0",Var "x_1"]
--
-- >>> let a_list = List [Var "a",Var "b"]
-- >>> let b_list = List [Var "c",Var "d"]
-- >>> rename "x_" 0 [] [a_list, b_list]
-- [List [Var "x_0_0",Var "x_0_1"],List [Var "x_1_0",Var "x_1_1"]]
--
rename :: String -> Int -> [Expr] -> [Expr] -> [Expr]
rename _ _ rs []
  = reverse rs
rename p i rs (v@(Var _):vs)
  = rename p (i + 1) ((Var $ p ++ show i) : rs) vs
rename p i rs ((List l):vs)
  = let
      r = List $ rename (p ++ (show i) ++ "_") 0 [] l
    in rename p (i + 1) (r:rs) vs

-- |
--
-- >>> apply (Map.fromList [(Var "x",Int 1)]) $ Var "x"
-- Int 1
--
-- >>> apply (Map.fromList [(Var "x",Int 1)]) $ List [Var "x",Var "x"]
-- List [Int 1,Int 1]
--
apply :: Map Expr Expr -> Expr -> Expr
apply t v@(Var _) = maybe v id $ Map.lookup v t
apply t (List xs) = List $ applyTraverse t xs []
apply t (Call f args) = Call f $ applyTraverse t args []

-- Other : Literal Value (Int 10, Float 0.5, String "foo", Boolean True)
apply t v = v

-- |
-- Traversal Apply
applyTraverse :: Map Expr Expr -> [Expr] -> [Expr] -> [Expr]
applyTraverse t [] rs     = reverse rs
applyTraverse t (v:vs) rs = applyTraverse t vs ((apply t v):rs)
