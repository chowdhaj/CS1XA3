{-|
Module      : ExprParser
Description : Parses strings into the 'Expr' datatype
Copyright   : (c) Jatin Chowdhary @2018
License     : WTFPL
Maintainer  : chowdhaj@mcmaster.ca
Stability   : Experimental
Portability : POSIX
This module will parse strings into the 'Expr' data type. It accepts Strings and will convert them into Doubles and Floats baesd on the String. 
-}

module ExprParser (parseExprD,parseExprF) where

import ExprType

import Text.Parsec.Char
import Text.Parsec.String

{- | parses an expression to Expr Double
     using the parsec package
-}

-- ^ parses an expression into Expr Double
parseExprD :: String        -- ^ The string to be parsed
           -> Expr Double   -- ^ The resulting parsed expression

parseExprD ss = case parse exprD "" ss of
   Left err   -> error $ show err
   Right expr -> expr

parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
   Left err   -> error $ show err
   Right expr -> expr


exprD :: Parser (Expr Double)
exprD = error "define me!" -- #TODO complete parser

exprF :: Parser (Expr Float)
exprF = error "define me!" -- #TODO complete parser