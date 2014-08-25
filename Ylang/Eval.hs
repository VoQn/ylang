module Ylang.Eval where

import Data.Set ((\\), Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Ylang.Syntax

-- |
-- Find Free Variables from Lambda Expression
--
-- (-> x x) ... []
-- >>> freeVars $ Lambda [Var "x"] (Var "x")
-- fromList []
--
-- (-> x y) ... [y]
-- >>> freeVars $ Lambda [Var "x"] (Var "y")
-- fromList [y]
--
-- (-> (x y) [w,x,y,z]) ... [w,z]
-- >>> let args = [Var "x",Var "y"]
-- >>> let expr = List [Var "w",Var "x",Var "y",Var "z"]
-- >>> freeVars $ Lambda args expr
-- fromList [w,z]
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
-- fromList [y]
--
-- ((-> y y) 1) ... []
-- >>> let func = Lambda [Var "y"] $ Var "y"
-- >>> freeVars $ Call func [Int 1]
-- fromList []
--
-- ((-> y x) 1) ... [x]
-- >>> let func = Lambda [Var "y"] $ Var "x"
-- >>> freeVars $ Call func [Int 1]
-- fromList [x]
--
freeVars :: Expr -> Set Expr
freeVars expr = case expr of
  v@(Var _)
    -> Set.singleton v

  List es
    -> collect es

  Define f args expr'
    -> (freeVars expr') \\ (Set.union (freeVars f) $ collect args)

  Lambda args expr'
    -> (freeVars expr') \\ (collect args)

  Call f args -> case f of
    g@(Lambda _ _)
      -> Set.union (freeVars g) $ collect args

    Operator _
      -> collect args

    v@(Var _)
      -> Set.insert v $ collect args

    Call g args'
      -> Set.union (freeVars g) $ collect args'
    _
      -> Set.empty
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
-- ((\ y_0) (((\ x_0) x_0) y_0))
--
-- Not-Convertable case:
-- (-> x ((-> y x) x)) ... (-> x ((-> y x) x))
-- >>> let g = Lambda [Var "y"] $ Var "x"
-- >>> let f = Lambda [Var "x"] $ Call g [Var "x"]
-- >>> alpha $ f
-- ((\ x) (((\ y) x) x))
--
alpha :: Expr -> Expr
alpha expr = case expr of
  f@(Lambda ys (Call g@(Lambda xs ex) _))
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
-- x
--
-- >>> let env  = Map.empty
-- >>> let func = Lambda [Var "x"] (Var "x")
-- >>> let expr = Call func [Var "y"]
-- >>> fst $ eval Map.empty expr
-- y
--
-- >>> let env = Map.fromList [(Var "x",Int 100)]
-- >>> fst $ eval env (Var "x")
-- 100
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
-- 10
--
applyf :: Expr -> [Expr] -> Expr
applyf expr args = case expr of
  Lambda (x@(Var _):[]) y@(Var _)
    | x == y      -> head args
    | elem y args -> y
    | otherwise   -> List []
  _ -> expr

-- |
-- Rename Variable identifier
--
-- >>> let vars = [Var "foo",Var "bar"]
-- >>> rename "x_" 0 [] vars
-- [x_0,x_1]
--
-- >>> let a_list = List [Var "a",Var "b"]
-- >>> let b_list = List [Var "c",Var "d"]
-- >>> rename "x_" 0 [] [a_list, b_list]
-- [[x_0_0 x_0_1],[x_1_0 x_1_1]]
--
rename :: String -> Int -> [Expr] -> [Expr] -> [Expr]
rename p i rs es = case es of
  []
    -> reverse rs
  v:vs
    -> let (j, r) = rename' p i v
       in rename p j (r:rs) vs
  where
  rename' :: String -> Int -> Expr -> (Int, Expr)
  rename' q k ex = case ex of
    Var _
      -> (k + 1, Var $ q ++ show k)

    List ys
      -> (k + 1, List $ rename (q ++ (show k) ++ "_") 0 [] ys)

    x -> (k, x)

-- |
--
-- >>> let env = Map.fromList [(Var "x",Int 1)]
-- >>> apply env $ Var "x"
-- 1
--
-- >>> let env = Map.fromList [(Var "x",Int 1)]
-- >>> apply env $ List [Var "x",Var "x"]
-- [1 1]
--
apply :: Map Expr Expr -> Expr -> Expr
apply env expr = case expr of
  v@(Var _) -> maybe v id $ Map.lookup v env
  List xs   -> List $ map (apply env) xs
  Call f args
    -> Call f $ map (apply env) args

  _ -> expr
