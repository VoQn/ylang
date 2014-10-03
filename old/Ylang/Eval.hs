module Ylang.Eval where

import Data.List (intercalate)
import qualified Data.Map as Map

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Ylang.Syntax
import Ylang.Value
import Ylang.Primitive
import Ylang.Display

-- OLD
type Eval b a
  = ReaderT b (ErrorT String
                (WriterT [String] (StateT b Identity))) a

type Result a b = ((Either String a, [String]), b)

getResult :: Result a b -> Either String a
getResult = fst . fst

getEnv :: Result a b -> b
getEnv = snd

runEval :: b -> Eval b a -> Result a b
runEval env evl =
  runIdentity (runStateT (runWriterT (runErrorT (runReaderT evl env))) env)
-- OLD


type Eval1 a
  = ReaderT Env1 (ErrorT String
                (WriterT [String] (StateT Env1 IO))) a

type Result1 a = ((Either String a, [String]), Env1)

runEval1 :: Env1 -> Eval1 a -> IO (Result1 a)
runEval1 env evl =
  runStateT (runWriterT (runErrorT (runReaderT evl env))) env

getResult1 :: IO (Result1 a) -> IO (Either String a)
getResult1 = fmap (fst . fst)

getEnv1 :: IO (Result1 a) -> IO (Env1)
getEnv1 = fmap snd

defPref :: String
defPref = "def_"

decPref :: String
decPref = "dec_"

variadicE f es = do
  vs <- mapM eval1 es
  case f vs of
    Right rs -> return rs
    Left   m -> throwError m

eval1 :: Expr -> Eval1 Val
eval1 Void        = return ValUnit
eval1 (Keyword k) = return $ ValKeyw k
eval1 (Boolean b) = return $ ValBool b
eval1 (Int     i) = return $ ValIntn i
eval1 (Float   f) = return $ ValFlon f
eval1 (Ratio   r) = return $ ValRatn r
eval1 (Char    c) = return $ ValChr c
eval1 (String  s) = return $ ValStr s

eval1 (Pair e1 e2) = ValPair <$> eval1 e1 <*> eval1 e2
eval1 (Array es)   = ValArray <$> mapM eval1 es

eval1 (Atom n) = do
  env <- get
  let key = defPref ++ n
  case Map.lookup key env of
    Just x  -> return x
    Nothing -> throwError $ "<Undefined Value> : " ++ n

-- add
eval1 (Call (Atom "+") es) = variadicE adds es

-- boolean operation
eval1 (Call (Atom "&") es) = variadicE ands es
eval1 (Call (Atom "|") es) = variadicE ors  es
eval1 (Call (Atom "^") es) = variadicE xors es
eval1 (Call (Atom "~") es) = variadicE nots es

eval1 _ = return $ ValBotm

eval :: Expr -> Eval Env0 Expr
  -- atomic
eval v@(Void     ) = return v
eval k@(Keyword _) = return k
eval b@(Boolean _) = return b
eval i@(Int     _) = return i
eval f@(Float   _) = return f
eval r@(Ratio   _) = return r
eval c@(Char    _) = return c
eval s@(String  _) = return s

  -- collection
eval (Pair e1 e2) = do
  r1 <- eval e1
  r2 <- eval e2
  return $ Pair r1 r2

eval (Array es) =
  mapM eval es >>= return . Array

eval (Atom n) = do
  tell [n]
  env <- get
  let key = defPref ++ n
  case Map.lookup key env of
    Just x -> return x
    Nothing ->
      throwError ("<Undefined Value> : " ++ n)

eval f@(Func _ _ _ _) = return f

eval d@(Declare n pm as r) = do
  env <- get
  let key = decPref ++ n
  case Map.lookup key env of
    Just (Declare _ q b l) ->
      let messages = [
              "<Conflict Definition>",
              "Already Defined ::",
              toString (Declare n q b l),
              "But Reassigned ::",
              toString (Declare n pm as r)
            ]
      in throwError $ intercalate " " messages
    Nothing -> do
      put $ Map.insert key d env
      return $ d

eval d@(Define n v) = do
  env <- get
  let key = defPref ++ n
  case Map.lookup key env of

    -- already assigned
    Just x ->
      let
        messages = [
            "<Conflict Definition>",
            "Already Defined ::",
            toString (Define n x),
            "But Reassigned ::",
            toString (Define n v)
          ]
        message = intercalate " " messages
      in throwError message

  -- can assign
    Nothing -> do
      v' <- eval v
      put $ Map.insert key v' env
      return $ d

  -- identity
eval (Call e []) = return e

eval _ = undefined
