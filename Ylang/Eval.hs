module Ylang.Eval where

import Data.List (intercalate)
import Data.Set ((\\), Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Ylang.Syntax

data Env = Env {
    name   :: Name
  , frames :: [Name]
  , scope  :: Map Expr Expr
  }

currentFrameName :: Env -> Name
currentFrameName (Env name frames _) = case frames of
  [] -> name
  _  -> intercalate "." (name:(reverse frames))

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
  -- void
  Factor [] -> (env, expr)

  -- identity
  Factor [e] -> eval env e

  -- apply
  Factor (f:es) ->
    let (env', result) = apply env f es
    in eval env' result

  _ -> (env, expr)

apply :: Env -> Expr -> [Expr] -> (Env, Expr)
apply e f [] = (e, f)
apply e f@(Atom n) (a:args) = case n of
  -- declare
  ":" -> undefined

  -- let
  "=" -> tryAssign e a args

  -- lambda
  "->" -> undefined

  -- cons
  "," -> undefined

  -- other (maybe function call)
  _ -> case Map.lookup f (scope e) of
    Nothing -> undefined
    Just g  -> (e, g)

apply env (Factor fs) (a:args) = undefined

apply _ _ _ = undefined

tryAssign _ _ [] = undefined -- TODO should throw Error
tryAssign e v@(Atom _) body = case Map.lookup v (scope e) of
  -- can assign
  Nothing -> case body of
    [x] -> (assign e v x, v)

    -- TODO many factor case
    _   -> undefined

  -- cannot reassign
  -- TODO should throw Error
  Just _ -> undefined

tryAssign e (Factor fs) body = undefined
