{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : ExprDiff
Description : Parses stuff
Copyright   : (c) Full Name @2018
License     : WTFPL
Maintainer  : Email@Domaind.ca
Stability   : experimental
Portability : POSIX
Much longer description comes here; write to your hearts content
-}

module ExprDiff where

import ExprType

import qualified Data.Map.Strict as Map

-- | This expression operates over the 'Expr' data type
class DiffExpr a where
  -- | Evaluate an expression var values
  eval :: Map.Map String a -> Expr a -> a
  -- | Simplify an expression and sub in values
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- | Perform partial differentiation w.r.t identifier
  partDiff :: String -> Expr a -> Expr a

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x


instance (Num a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  simplify _ e = e -- #TODO finish me!
  partDiff _ e = e -- #TODO finish me!
