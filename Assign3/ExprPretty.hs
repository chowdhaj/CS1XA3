{-|
Module      : ExprPretty
Description : Converts evaluation of expressions to a friendly format that is easy to interpret
Copyright   : (c) Jatin Chowdhary @2018
License     : WTFPL
Maintainer  : chowdhaj@mcmaster.ca
Stability   : Experimental
Portability : POSIX
This module serves to provide a differet implementation for 'Show' for the 'Expr' datatype. It converts the output of 'ExprDiff' to a format that is easy to read for humans. This is done by using common mathematical symbols to represent the expressions. However, not everyone will find this format more readable. For example: Mark Zuckerburg, the privacy invader. Seriously, I highly doubt he is a human. He has to be a lizard... Or a cyborg... Or a lizard cyborg.
-}

module ExprPretty where

import ExprType

{-
parens : Wraps values inside brackets/quotations
0 = ( Round Brackets )
1 = [ Square Brackets ]
2 = { Curly Brackets }
3 = " Double Quotes "
4 = ' Single Quotes '
-}
parens :: (Eq a, Num a) => a -> [Char] -> [Char]
parens n ss = if n == 0 then "(" ++ ss ++ ")" else
  if n == 1 then "[" ++ ss ++ "]" else
  if n == 2 then "{" ++ ss ++ "}" else
  if n == 3 then "\'" ++ ss ++ "\'" else 
  "\"" ++ ss ++ "\""

{- 
Instance Show Expr
  Outputs a readable (human-friendly) of our datatype 'Expr'
  Each numeric expression has its own implementation of 'show'
-}
instance Show a => Show (Expr a) where
  show (E e)         = " (Natural) e ^ " ++ (parens 0 (show e))
  show (Add e1 e2)   = (parens 0 (show e1)) ++ " !+ " ++ (parens 0 (show e2))
  show (Sub e1 e2)   = (parens 0 (show e1)) ++ " !- " ++ (parens 0 (show e2))
  show (Sin e)       = " Sin " ++ (parens 0 (show e))
  show (Cos e)       = " Cos " ++ (parens 0 (show e))
  show (Log e)       = " Log Base e Of" ++ (parens 0 (show e))
  show (Exp e1 e2)   = (parens 0 (show e1)) ++ " !^ " ++ (parens 0 (show e2))
  show (Var ss)      = parens 0 ("var " ++ (parens 4 (show ss))) -- show (Var ss) = parens $ "var \"" ++ ss ++ "\""
  show (Const x)     = (parens 0 ("val" ++ show x)) -- show (Const x) = parens $ "val " ++ show x
  show (Multi e1 e2) = (parens 0 (show e1)) ++ " !* " ++ (parens 0 (show e2))
