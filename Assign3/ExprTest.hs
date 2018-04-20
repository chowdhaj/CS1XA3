{-|
Module      : ExprTest
Description : Combines all modules into a complete math library utility
Copyright   : (c) Jatin Chowdhary @2018
License     : WTFPL
Maintainer  : chowdhaj@mcmaster.ca
Stability   : Experimental
Portability : POSIX
This module imports all other modules, combining them into a working math library that will help you do your calculus homework. It even has a few test cases to try it out.
-}

module ExprTest where

import ExprDiff
import ExprParser
import ExprPretty
import ExprType

import qualified Data.Map.Strict as Map
import Test.QuickCheck

sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")

listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

test1 :: Int -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
