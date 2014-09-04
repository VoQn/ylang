{-# LANGUAGE BangPatterns #-}
module Ylang.Primitive
  (
    addBin,
    andBin,
    orBin,
    xorBin,
    notUnary,
    adds,
    ands,
    ors,
    xors,
    nots
  ) where

import Ylang.Value

type BinOp a = a -> a -> Either String a
type Variadic a = [a] -> Either String a

variadic :: BinOp Val -> Variadic Val
variadic _ [x] = Right x
variadic f (x1:x2:[]) = f x1 x2
variadic f (x1:x2:xs) = case (f x1 x2) of
  Right x -> variadic f (x:xs)
  Left  e -> Left e

variadicHalt :: BinOp Val -> Val -> Variadic Val
variadicHalt _ _ [x] = Right x
variadicHalt f t (x1:x2:[]) = f x1 x2
variadicHalt f t (x1:x2:xs) = case (f x1 x2) of
  Right x
    | x == t    -> Right t
    | otherwise -> variadic f (x:xs)
  Left  e -> Left e

undefinedFound :: Either String Val
undefinedFound = Left "Undefined"

unknownImplError :: String -> Val -> Either String Val
unknownImplError fn x = Left $
  "Undefined Implement " ++ fn ++ " for " ++ showType x ++ " type"

typeNotMatch :: Either String Val
typeNotMatch = Left "Type Not Match"

-- |
-- (+ <ylang-value> <ylang-value>)
addBin :: BinOp Val
addBin x y = case (x, y) of
  -- Numbers
  (ValIntn i, ValIntn j) -> Right $ ValIntn (i + j)
  (ValFlon i, ValFlon j) -> Right $ ValFlon (i + j)
  (ValRatn i, ValRatn j) -> Right $ ValRatn (i + j)

  (ValIntn i, ValFlon j) -> Right $ ValRatn (fromInteger i + toRational j)
  (ValIntn i, ValRatn j) -> Right $ ValRatn (fromInteger i + j)
  (ValFlon i, ValRatn j) -> Right $ ValRatn (toRational  i + j)
  (ValFlon _, ValIntn _) -> addBin y x
  (ValRatn _, ValIntn _) -> addBin y x
  (ValRatn _, ValFlon _) -> addBin y x

  (ValChr a, ValChr b) -> Right $ ValStr (a:b:[])
  (ValChr a, ValStr b) -> Right $ ValStr (a:b)
  (ValStr a, ValChr b) -> Right $ ValStr (a ++ [b])
  (ValStr a, ValStr b) -> Right $ ValStr (a ++ b)

  (ValBotm, _) -> undefinedFound
  (_, ValBotm) -> undefinedFound
  (_, _)
    | getType x == getType y -> unknownImplError "(+)" x
    | otherwise -> typeNotMatch

adds :: Variadic Val
adds = variadic addBin

andBin :: BinOp Val
andBin x y = case (x, y) of
  (ValBool i, ValBool j) -> Right $ ValBool (i && j)

  (ValBotm, _) -> undefinedFound
  (_, ValBotm) -> undefinedFound
  (_, _)
    | getType x == getType y -> unknownImplError "(&)" x
    | otherwise -> typeNotMatch

ands :: Variadic Val
ands = variadicHalt andBin $ ValBool False

orBin :: BinOp Val
orBin x y = case (x, y) of
  (ValBool i, ValBool j) -> Right $ ValBool (i || j)

  (ValBotm, _) -> undefinedFound
  (_, ValBotm) -> undefinedFound
  (_, _)
    | getType x == getType y -> unknownImplError "(|)" x
    | otherwise -> typeNotMatch

ors :: Variadic Val
ors = variadicHalt orBin $ ValBool True

xorBin :: BinOp Val
xorBin x y = case (x, y) of
  (ValBool True, ValBool False) -> Right $ ValBool True
  (ValBool False, ValBool True) -> Right $ ValBool True
  (ValBool _, ValBool _) -> Right $ ValBool False

  (ValBotm, _) -> undefinedFound
  (_, ValBotm) -> undefinedFound
  (_, _)
    | getType x == getType y -> unknownImplError "(^)" x
    | otherwise -> typeNotMatch

xors :: Variadic Val
xors = variadic xorBin

notUnary :: Val -> Either String Val
notUnary (ValBool i) = Right $ ValBool (not i)
notUnary ValBotm = undefinedFound
notUnary _ = typeNotMatch

nots :: Variadic Val
nots (e:[]) = notUnary e
nots (e:es) = Left "Too Parameter"
