module ExprType where

import Data.List

-- * Section: DataType Declaration

-- | A datatype for encoding numeric expressions
data Expr a = Add (Expr a) (Expr a)    -- ^ Binary addition
            | Mult (Expr a) (Expr a)   -- ^ Binary multiplication
            | Const a                  -- ^ Wrap a constant value
            | Var String               -- ^ Wrap a variable identifier
  deriving Eq

-- * Section: Auxiliary Functions
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]

