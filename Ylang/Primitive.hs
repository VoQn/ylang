{-# LANGUAGE BangPatterns #-}
module Ylang.Primitive
  (
    addBin,
    adds
  ) where

import Ylang.Value

-- |
-- (+ <ylang-value> <ylang-value>)
addBin :: Val -> Val -> Either String Val
addBin x y = case (x, y) of
  -- Numbers
  (ValIntn i, ValIntn j) -> Right $ ValIntn (i + j)
  (ValFlon i, ValFlon j) -> Right $ ValFlon (i + j)
  (ValRatn i, ValRatn j) -> Right $ ValRatn (i + j)

  (ValBotm, _) -> Left "Undefined"
  (_, ValBotm) -> Left "Undefined"
  _ -> Left "Type Not Match"

adds :: [Val] -> Either String Val
adds [x] = Right x
adds (x1:x2:xs) = case x1 `addBin` x2 of
  Right x -> adds (x:xs)
  Left  e -> Left e
