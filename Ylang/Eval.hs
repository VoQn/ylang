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
-- >>> freeVars $ Lambda (Atom "x") [] (Atom "x")
-- fromList []
--
-- (-> x y) ... [y]
-- >>> freeVars $ Lambda (Atom "x") [] (Atom "y")
-- fromList [y]
--
-- (-> (x y) [w,x,y,z]) ... [w,z]
-- >>> freeVars $ Lambda (Atom "x") [Atom "y"] $ Array [Atom "w",Atom "x",Atom "y",Atom "z"]
-- fromList [w,z]
--
-- (-> x (+ x 1))
-- >>> freeVars $ Lambda (Atom "x") [] $ Call (Atom "+") [Atom "x",Int 1]
-- fromList [+]
--
-- (-> x (+ y 1))
-- >>> freeVars $ Lambda (Atom "x") [] $ Call (Atom "+") [Atom "y",Int 1]
-- fromList [+,y]
--
-- ((-> y y) 1) ... []
-- >>> freeVars $ Call (Lambda (Atom "y") [] $ Atom "y") [Int 1]
-- fromList []
--
-- ((-> y x) 1) ... [x]
-- >>> freeVars $ Call (Lambda (Atom "y") [] $ Atom "x") [Int 1]
-- fromList [x]
--
freeVars :: Expr -> Set Expr
freeVars expr = case expr of
  Atom _
    -> Set.singleton expr

  Array es
    -> collect es

  Define f args expr'
    -> (freeVars expr') \\ (Set.union (freeVars f) $ collect args)

  Lambda i args expr'
    -> (freeVars expr') \\ (collect (i:args))

  Call f args -> case f of
    Lambda _ _ _
      -> Set.union (freeVars f) $ collect args

    Atom _
      -> Set.insert f $ collect args

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
-- >>> let g = Lambda (Atom "x") [] $ Atom "x"
-- >>> let f = Lambda (Atom "x") [] $ Call g [Atom "x"]
-- >>> alpha $ f
-- (-> y_0 ((-> x_0 x_0) y_0))
--
-- Not-Convertable case:
-- (-> x ((-> y x) x)) ... (-> x ((-> y x) x))
-- >>> let g = Lambda (Atom "y") [] $ Atom "x"
-- >>> let f = Lambda (Atom "x") [] $ Call g [Atom "x"]
-- >>> alpha $ f
-- (-> x ((-> y x) x))
--
alpha :: Expr -> Expr
alpha expr = case expr of
  f@(Lambda y ys (Call g@(Lambda x xs ex) _))
    | hasNotOutScopeBind g ->
        let
          (x':xs') = rename "x_" 0 [] (x:xs)
          (y':ys') = rename "y_" 0 [] (y:ys)
          ex' = apply (Map.fromList $ zip (x:xs) (x':xs')) ex
        in Lambda y' ys' $ Call (Lambda x' xs' ex') (y':ys')
    | otherwise -> f
  _
    -> expr
  where
  hasNotOutScopeBind g = Set.null $ freeVars g

builtins :: Map Expr Expr
builtins = Map.fromList
  [
      (Atom "id", Lambda (Atom "x") [] (Atom "x"))
    , (Atom "seq", Lambda (Atom "x") [Atom "y"] (Atom "y"))
  ]

-- |
-- Evaluate Expression
--
-- >>> let env = Map.empty
-- >>> snd $ eval env (Atom "x")
-- x
--
-- >>> let env  = Map.empty
-- >>> let func = Lambda (Atom "x") [] (Atom "x")
-- >>> let expr = Call func [Atom "y"]
-- >>> snd $ eval Map.empty expr
-- y
--
-- >>> let env = Map.fromList [(Atom "x",Int 100)]
-- >>> snd $ eval env (Atom "x")
-- 100
--
eval :: Map Expr Expr -> Expr -> (Map Expr Expr, Expr)
eval env (Call f args) = eval env $ applyf f args
eval env expr =
  let expr' = maybe expr id $ Map.lookup expr env
  in (env, expr')

-- |
-- Apply Function with Arguments
--
-- id : (-> x x)
-- >>> applyf (Lambda (Atom "x") [] (Atom "x")) [Int 10]
-- 10
--
applyf :: Expr -> [Expr] -> Expr
applyf expr args = case expr of
  Lambda x@(Atom _) [] y@(Atom _)
    | x == y      -> head args
    | elem y args -> y
    | otherwise   -> Array []
  _ -> expr

-- |
-- Rename Variable identifier
--
-- >>> let vars = [Atom "foo",Atom "bar"]
-- >>> rename "x_" 0 [] vars
-- [x_0,x_1]
--
-- >>> let a_list = Array [Atom "a",Atom "b"]
-- >>> let b_list = Array [Atom "c",Atom "d"]
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
    Atom _
      -> (k + 1, Atom $ q ++ show k)

    Array ys
      -> (k + 1, Array $ rename (q ++ (show k) ++ "_") 0 [] ys)

    x -> (k, x)

-- |
--
-- >>> let env = Map.fromList [(Atom "x",Int 1)]
-- >>> apply env $ Atom "x"
-- 1
--
-- >>> let env = Map.fromList [(Atom "x",Int 1)]
-- >>> apply env $ Array [Atom "x",Atom "x"]
-- [1 1]
--
apply :: Map Expr Expr -> Expr -> Expr
apply env expr = case expr of
  v@(Atom _) -> maybe v id $ Map.lookup v env
  Array xs   -> Array $ map (apply env) xs
  Call f args
    -> Call f $ map (apply env) args

  _ -> expr
