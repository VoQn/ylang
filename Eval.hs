module Eval where

import Data.List (intercalate)
import Data.Set ((\\), Set, fromList)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax

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
yexpr (Var v)      = v
yexpr (Boolean b)  = if b then "Yes" else "No"
yexpr (Int n)      = show n
yexpr (Float f)    = show f
yexpr (String s)   = s
yexpr (Operator p) = p

yexpr (List es)
  = ('[':(yexpr' es)) ++ "]"

yexpr (Lambda xs b)
  = "(-> (" ++ (yexpr' xs) ++ ") " ++ (yexpr b) ++ ")"

yexpr (Call f args)
  = '(' : (yexpr f) ++ " " ++ (yexpr' args) ++ ")"

yexpr expr = show expr

yexpr' (e:es) = intercalate " " $ map yexpr (e:es)

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
-- >>> freeVars $ Lambda [Var "x",Var "y"] $ List [Var "w",Var "x",Var "y",Var "z"]
-- fromList [Var "w",Var "z"]
--
-- (-> x (+ x 1))
-- >>> freeVars $ Lambda [Var "x"] $ Call (Operator "+") [Var "x",Int 1]
-- fromList []
--
-- (-> x (+ y 1))
-- >>> freeVars $ Lambda [Var "x"] $ Call (Operator "+") [Var "y",Int 1]
-- fromList [Var "y"]
--
-- ((-> y y) 1) ... []
-- >>> freeVars $ Call (Lambda [Var "y"] (Var "y")) [Int 1]
-- fromList []
--
-- ((-> y x) 1) ... [x]
-- >>> freeVars $ Call (Lambda [Var "y"] (Var "x")) [Int 1]
-- fromList [Var "x"]
--
freeVars :: Expr -> Set Expr
freeVars v@(Var _)
  = Set.singleton v

freeVars (List l)
  = freeVars' l

freeVars (Define name args expr)
  = (freeVars expr) \\ (Set.insert (Var name)  $ freeVars' args)

freeVars (Lambda args expr)
  = (freeVars expr) \\ (freeVars' args)

freeVars (Call g@(Lambda _ _) args)
  = Set.union (freeVars g) (freeVars' args)

freeVars (Call (Operator _) args)
  = freeVars' args

freeVars (Call v@(Var _) args)
  = Set.insert v $ freeVars' args

freeVars expr = Set.empty

freeVars' exps = col Set.empty exps
  where
  col rs []     = rs
  col rs (v:vs) = col (Set.union (freeVars v) rs) vs

-- |
-- Alpha Conversion for Lambda Calculus
--
-- (-> x ((-> x x) x)) ... (-> y ((-> x x) y))
-- >>> alpha $ Lambda [Var "x"] (Call (Lambda [Var "x"] (Var "x")) [Var "x"])
-- Lambda [Var "y_0"] (Call (Lambda [Var "x_0"] (Var "x_0")) [Var "y_0"])
--
-- (-> x ((-> y x) x)) ... (-> x ((-> y x) x))
-- >>> alpha $ Lambda [Var "x"] (Call (Lambda [Var "y"] (Var "x")) [Var "x"])
-- Lambda [Var "x"] (Call (Lambda [Var "y"] (Var "x")) [Var "x"])
--
alpha :: Expr -> Expr
alpha f@(Lambda ys (Call g@(Lambda xs ex) zs))
  | possible =
      let
        xs' = rename "x_" 0 [] xs
        ys' = rename "y_" 0 [] ys
        ex' = apply (Map.fromList $ zip xs xs') ex
      in Lambda ys' $ Call (Lambda xs' ex') ys'
  | otherwise = f
  where
  possible = Set.null $ freeVars g

alpha l = l

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
