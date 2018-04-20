{-|
Module      : ExprType
Description : Contains the 'Expr' datatype that defines constructors to model our mathematical data
Copyright   : (c) Jatin Chowdhary @2018
License     : WTFPL
Maintainer  : chowdhaj@mcmaster.ca
Stability   : Experimental
Portability : POSIX
The 'Expr' datatype allows us to model mathematical data which enables evaluating expressions, performing differentiation, and simplification of expressions. This class was amde for encoding numeric expressions. 
-}

module ExprType where

import Data.List

-- * Section: DataType Declaration

-- | A datatype for encoding numeric expressions. Each data is of type 'Expr a', with the exception of Const
data Expr a = E (Expr a)              -- ^ Represents the natural exponent (e)
	        | Add (Expr a) (Expr a)   -- ^ Represents binary addition by wrapping two 'Expr a' expressions in 'Add'
            | Sub (Expr a) (Expr a)   -- ^ Represents binary subtraction by wrapping two 'Expr a' expressions in 'Sub'
            | Sin (Expr a)            -- ^ Represents the trignometric function, sine (sin)
            | Cos (Expr a)            -- ^ Represents the trignometric function, cosine (cos)
            | Log (Expr a)            -- ^ Represents log base of 'e' of 'Expr a'. Base natural e is the default base.
            | Exp (Expr a) (Expr a)   -- ^ Represents exponents, where the first 'Expr a' is the base, and the second 'Expr a' is the power
            | Var String              -- ^ Represents a variable of type String
            | Const a                 -- ^ Represents a constant of value 'a', where 'a' can be any number
            | Multi (Expr a) (Expr a) -- ^ Represents binary multiplication by wrapping two 'Expr a' expressions in 'Multi'
  deriving (Eq, Show) -- I'm gonna let GHCi do all the work for "Show".
-- But there's a module called ExprPretty.hs that'll make the output look more readable; use it at your own discretion

-- * Section: Auxiliary Functions

{- getVars : Extracts and returns variables from 'Expr' -}
getVars :: Expr a -> [String]
getVars (E e)         = getVars e
getVars (Add e1 e2)   = getVars e1 `union` getVars e2
getVars (Sub e1 e2)   = getVars e1 `union` getVars e2
getVars (Sin e)       = getVars e
getVars (Cos e)       = getVars e
getVars (Log e)       = getVars e
getVars (Exp e1 e2)   = getVars e1 `union` getVars e2
getVars (Var ident)   = [ident]
getVars (Const _)     = []
getVars (Multi e1 e2) = getVars e1 `union` getVars e2
