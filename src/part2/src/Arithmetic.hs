-- This is a skeleton file for you to edit
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x) | x >= 0 = show x
showExp (Cst x) | x < 0 = "(" ++ show x  ++ ")"
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "`div`" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _ = error "showExp can not handle complex expressions!"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) = evalSimple e1 `div` evalSimple e2
evalSimple (Pow e1 e2) 
  | evalSimple e2 == 0 = evalSimple e1 * 0 + 1
  | evalSimple e2 > 0  = evalSimple e1 ^ evalSimple e2
  | otherwise          = error "The exponent should be a nonnegative integer."
evalSimple _ = error "evalSimple can not handle complex expressions!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv vn value oldEnv v = if v == vn then Just value else oldEnv v

evalFull :: Exp -> Env -> Integer
evalFull (Cst v) _ = v
evalFull (Add e1 e2) env = evalFull e1 env + evalFull e2 env
evalFull (Sub e1 e2) env = evalFull e1 env - evalFull e2 env
evalFull (Mul e1 e2) env = evalFull e1 env * evalFull e2 env
evalFull (Div e1 e2) env = evalFull e1 env `div` evalFull e2 env
evalFull (Pow e1 e2) env = if e2result == 0 then e1result * 0 + 1
  else  e1result ^ e2result
  where 
    e1result =  evalFull e1 env
    e2result = evalFull e2 env
    
evalFull (If test yes no) env = if evalFull test env /= 0
  then evalFull yes env
  else evalFull no env

evalFull (Var vname) env = case env vname of
  Just v -> v
  Nothing -> error (vname ++ " has no current value!")

evalFull (Let var def body) env = evalFull body newEnv
  where
    value = evalFull def env
    newEnv = extendEnv var value env
evalFull (Sum var from to body) env  
  | fromValue > toValue = 0
  | otherwise = foldl (\acc value -> acc + evalFull body (extendEnv var value env)) 0 possibleValueList
  where
    fromValue = evalFull from env
    toValue = evalFull to env
    possibleValueList = [fromValue..toValue]

-- implement f to Either a and Either b
-- for example : 
-- monadHelper (+) (Right 1) (Right 2) = Right (1+2) = Right 3
-- used in evalErr
monadHelper :: (Integer -> Integer -> Integer)
  -> Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
monadHelper f e1 e2 =
  e1 >>= \x ->
  e2 >>= \y -> Right (f x y)

wrongExp :: Either ArithError Integer -> Bool
wrongExp (Right _) = False
wrongExp (Left _) = True

lessForEither :: Either ArithError Integer -> Either ArithError Integer -> Bool
lessForEither (Left _) _ = False
lessForEither _ (Left _) = False
lessForEither (Right x) (Right y) = x < y

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst v) _ = Right v

evalErr (Add e1 e2) env = monadHelper (+) (evalErr e1 env) (evalErr e2 env)
evalErr (Sub e1 e2) env = monadHelper (-) (evalErr e1 env) (evalErr e2 env)
evalErr (Mul e1 e2) env = monadHelper (*) (evalErr e1 env) (evalErr e2 env)

evalErr (Div e1 e2) env
  | e2ValueErr == Right 0 = Left EDivZero
  | otherwise = monadHelper div (evalErr e1 env) e2ValueErr
  where e2ValueErr = evalErr e2 env

evalErr (Pow e1 e2) env
  | lessForEither e2ValueErr (Right 0) = Left ENegPower
  | otherwise = monadHelper (^) (evalErr e1 env) e2ValueErr
  where e2ValueErr = evalErr e2 env

evalErr (Var vname) env = case env vname of
  Nothing ->  Left (EBadVar vname)
  Just x ->  Right x

evalErr (If test yes no) env = case evalErr test env of
  Right x -> case x of
    0 -> evalErr no env
    _ -> evalErr yes env
  Left x -> Left x

evalErr (Let var def body) env = case evalErr def env of
  Right x -> evalErr body (extendEnv var x env)
  Left x -> Left x

evalErr (Sum var from to body) env
  | wrongExp fromErr = fromErr
  | wrongExp toErr = toErr
  | f > t = Right 0
  | errInBody = wrongBodyResult
  | otherwise = rightBodyResult
  where
    fromErr = evalErr from env
    toErr = evalErr to env
    f = evalFull from env
    t = evalFull to env
    possibleValues = [f..t]
    resultList = map (\value -> evalErr body (extendEnv var value env)) [f..t]
    errInBody = foldl (\acc result -> wrongExp result || acc) False resultList
    wrongBodyResult = head $ filter wrongExp resultList
    rightBodyResult = Right (foldl (\acc value -> evalFull body (extendEnv var value env) + acc) 0 possibleValues)

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
