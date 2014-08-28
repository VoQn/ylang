module Ylang.Eval where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Ylang.Syntax

data Env = Env {
    name   :: Name
  , frames :: [Name]
  , scope  :: Map Expr Expr
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

assign :: Env -> Expr -> Expr -> Env
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
  -- void
  Factor [] -> (env, expr)

  -- identity
  Factor (e:[]) -> eval env e

  -- applycation
  Factor (f:args) -> case f of
    -- declare (type binding)
    Atom ":"  -> declaration env args

    -- define (value binding)
    Atom "="  -> definition  env args

    -- create function (not evaluate this time)
    Atom "->" -> anonymous   env args

    -- other atomic something (maybe function)
    g@(Atom _) ->
      let (env', h) = apply env g args
      in eval env' h

    -- nested expression
    Factor (g:args') ->
      let (env', h) = apply env g args'
      in eval env' (Factor (h:args))

  Atom _ -> case Map.lookup expr (scope env) of
    Just a  -> (env, a)

    -- TODO undefineded atom case
    Nothing -> undefined

  _ -> (env, expr)

declaration :: Env -> [Expr] -> (Env, Expr)
declaration = undefined

definition :: Env -> [Expr] -> (Env, Expr)
definition e (v@(Atom _):body) = case Map.lookup v (scope e) of
  -- can assign
  Nothing -> case body of
    [x] -> (assign e v x, v)

    -- TODO many factor case
    _   -> undefined

  -- cannot reassign
  -- TODO should throw Error
  Just _ -> undefined

anonymous :: Env -> [Expr] -> (Env, Expr)
anonymous = undefined

apply :: Env -> Expr -> [Expr] -> (Env, Expr)
apply e f [] = (e, f)

apply e f@(Atom _) args = case Map.lookup f (scope e) of
  Nothing -> undefined
  Just g  ->
    let (e', h) = eval e g
    in eval e' (Factor (h:args))

apply _ _ _ = undefined
