module ExprType where

import Data.List

-- * Section: DataType Declaration

-- | A datatype for encoding numeric expressions
data Expr a = Add (Expr a) (Expr a)   -- ^ Binary addition
            | Multi (Expr a) (Expr a) -- ^ Binary multiplication
            | Sin (Expr a)            -- ^ Trignometry Sine (sin)
            | Cos (Expr a)            -- ^ Trignometry Cosine (cos)
            | Log (Expr a)            -- ^ Log Base Of 'a'
            | Exp (Expr a)            -- ^ Natural Exponent (e)
            | Const a                 -- ^ Wrap a constant value
            | Var String              -- ^ Wrap a variable identifier
  deriving (Eq, Show) -- 



-- * Section: Auxiliary Functions
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Multi e1 e2) = getVars e1 `union` getVars e2
getVars (Sin e1) = getVars e1
getVars (Cos e1) = getVars e1
getVars (Log e1) = getVars e1
getVars (Exp e1) = getVars e1
getVars (Const _)    = []
getVars (Var ident)  = [ident]
