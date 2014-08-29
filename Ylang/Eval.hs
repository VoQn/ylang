module Ylang.Eval where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Ylang.Syntax hiding (name)

data Env = Env {
    name   :: Name
  , frames :: [Name]
  , scope  :: Map Name Expr
  }

currentFrameName :: Env -> Name
currentFrameName (Env n fs _) = case fs of
  [] -> n
  _  -> intercalate "." (n:(reverse fs))

defaultEnv :: Env
defaultEnv = Env {
      name   = "TopLevel"
    , frames = []
    , scope  = Map.empty
  }

assign :: Env -> Name -> Expr -> Env
assign (Env n f s) sym expr =
  let r = Map.insert sym expr s
  in Env { name = n, frames = f, scope = r }

eval :: Env -> Expr -> (Env, Expr)
eval env expr = case expr of
  -- atomic
  Boolean _ -> (env, expr)
  Int     _ -> (env, expr)
  Float   _ -> (env, expr)
  Ratio   _ -> (env, expr)
  String  _ -> (env, expr)

  -- collection
  Pair _ _ -> (env, expr)
  Array  _ -> (env, expr)

  -- factor
  Define n v -> definition env n v

  -- void
  Factor [] -> (env, expr)

  -- identity
  Factor (e:[]) -> eval env e

  -- applycation
  Factor (f:args) -> case f of
    -- declare (type binding)
    Atom ":"  -> declaration env args

    -- create function (not evaluate this time)
    Func a as es r -> anonymous env (a:as) es r args

    -- other atomic something (maybe function)
    g@(Atom _) ->
      let (env', h) = apply env g args
      in eval env' h

    -- nested expression
    Factor (g:args') ->
      let (env', h) = apply env g args'
      in eval env' (Factor (h:args))

  Atom n -> case Map.lookup n (scope env) of
    Just a  -> (env, a)

    -- TODO undefineded atom case
    Nothing -> undefined

  _ -> (env, expr)

declaration :: Env -> [Expr] -> (Env, Expr)
declaration = undefined

definition :: Env -> Name -> Expr -> (Env, Expr)
definition e v body = case Map.lookup v (scope e) of
  -- can assign
  Nothing -> let e' = assign e v body in (e', Atom v)

  -- cannot reassign
  -- TODO should throw Error
  Just _ -> undefined

-- anonymous :: Env -> Expr -> [Expr] -> (Env, Expr)
anonymous e vs es r as
  | length vs == length as = -- apply and return
      undefined

  | length vs > length as = -- partial apply
      undefined

  | otherwise = -- arity over
      undefined

apply :: Env -> Expr -> [Expr] -> (Env, Expr)
apply e f [] = (e, f)

apply e (Atom n) args = case Map.lookup n (scope e) of
  Nothing -> undefined
  Just g  ->
    let (e', h) = eval e g
    in eval e' (Factor (h:args))

apply _ _ _ = undefined
