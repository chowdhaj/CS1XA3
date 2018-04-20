{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : ExprDiff
Description : Performs operations (i.e. Simplification, Differentiation, & Evaluation) on 'Expr' expressions
Copyright   : (c) Jatin Chowdhary @2018
License     : WTFPL
Maintainer  : chowdhaj@mcmaster.ca
Stability   : Experimental
Portability : POSIX
All operations and evaluations are performed here. The implementations for simplification, differentiation, & evaluation can be found in this module. Details on the procedure can be read below. All rules for the 'Expr' data type are defined below.
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

  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2
  
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Multi e1 e2
  
  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2

  val :: a -> Expr a
  val x = Const x
  
  var :: String -> Expr a
  var x = Var x
  
  myCos :: Expr a -> Expr a 
  myCos x = Cos x
  
  mySin :: Expr a -> Expr a 
  mySin x = Sin x
  
  myLog :: Expr a -> Expr a 
  myLog x = Log x
  
  myE :: Expr a -> Expr a 
  myE x = E x
  
  myExp :: Expr a -> Expr a -> Expr a
  myExp x y = Exp x y 

  -- | The 'Answer' datatype stores the value of an expression based on the final answer
  data Answer a = Error String -- ^ Wraps a 'String' in 'Error' if something goes wrong with the expression
                | Answer a     -- ^ Wraps the right answer in 'Answer' if the expression is computed

instance (Floating a) => DiffExpr a where

  eval vrs (E e)                           = if e <= 0 then error "This Is Mathematically Impossible" else exp (eval vrs e)
  eval vrs (Add e1 e2)                     = eval vrs e1 + eval vrs e2
  eval vrs (Sub e1 e2)                     = eval vrs e1 - eval vrs e2
  eval vrs (Sin e)                         = sin (eval vrs e)
  eval vrs (Cos e)                         = cos (eval vrs e)
  eval vrs (Log e)                         = log (eval vrs e)
  eval vrs (Exp e1 e2)                     = (eval vrs e1) ** (eval vrs e2)
  eval vrs (Var x)                         = case Map.lookup x vrs of
                                  Just v   -> v
                                  Nothing  -> error "Sorry, Lookup Failed. Perhaps You Made A Mistake"
  eval vrs (Const x)                       = x
  eval vrs (Multi e1 e2)                   = eval vrs e1 * eval vrs e2  

  partDiff p (E e)                         = Multi (E e) (partDiff p e)
  partDiff p (Add e1 e2)                   = Add (partDiff p e1) (partDiff p e2)
  partDiff p (Sub e1 e2)                   = Sub (partDiff p e1) (partDiff p e2)
  partDiff p (Sin x)                       = Multi (Cos x) (partDiff p x) 
  partDiff p (Cos x)                       = Multi (Neg (Sin x)) (partDiff p x)
  partDiff p (Log a b)                     = Multi (Div (Const 1) (Ln (Const a))) (partDiff p b)
  partDiff p (Exp a b)                     = Mult (Mult b (Exp a (Add b (Const (-1))))) (partDiff p a)
  partDiff p (Var e)                       = if e == p then (Const 1) else (Const 0)
  partDiff _ (Const _)                     = Const 0
  partDiff s (Multi e1 e2)                 = Add (Multi (partDiff s e1) e2) (Multi e1 (partDiff s e2))

  simplify vrs (E (Const 0) e)             = Const 1 
  simplify vrs (E (Const a) e)             = case (eval vrs (E Const a))) of
                                Answer n   -> Const n
                                Error e    -> E (Const a)
  simplify vrs (E (Var x) e)               = E (simplify vrs e)
  simplify vrs (E (Log x) e)               = x
  simplify vrs (E (Multi x (Log y)) e)     = Pow x y
  simplify vrs (E (x) e)                   = x
  
  simplify vrs (Add (Const 0) e)           = simplify vrs e
  simplify vrs (Add e (Const 0))           = simplify vrs e
  simplify vrs (Add (Const e1) (Const e2)) = Const (e1 - e2)
  simplify vrs (Add (Var e1) (Var e2))     = case (Map.lookup x vrs) of
                                Just n     -> case (Map.lookup e2 vrs) of
                                  Just m   -> Const (eval vrs (Add (Var e1) (Var e2)))
                                  Nothing  -> Add (simplify vrs (Var e1)) (simplify vrs (Var e2))
                                Nothing    -> Add (simplify vrs (Var e1)) (simplify vrs (Var e2))
  simplify vrs (Add (Var e1) (Const e2))   = case Map.lookup e1 vrs of
                                  Just n   -> Const (eval vrs (Add (Var e1) (Const e2)))
                                  Nothing  -> Add (Var e1) (Const e2)
  simplify vrs (Add (Const e1) (Var e2))   = case Map.lookup e2 vrs of
                                  Just y   -> Const (eval vrs (Add (Var e2) (Const e1)))
                                  Nothing  -> Add (Const e1) (Var e2) 
  simplify vrs (Add e1 e2)                 = Add (simplify vrs e1) (simplify vrs e2)

  simplify vrs (Sub (Const 0) e)           = simplify vrs e
  simplify vrs (Sub e (Const 0))           = simplify vrs e
  simplify vrs (Sub (Const e1) (Const e2)) = Const (e1 - e2)
  simplify vrs (Sub (Var e1) (Var e2))     = case (Map.lookup x vrs) of
                                Just n     -> case (Map.lookup e2 vrs) of
                                  Just m   -> Const (eval vrs (Sub (Var e1) (Var e2)))
                                  Nothing  -> Sub (simplify vrs (Var e1)) (simplify vrs (Var e2))
                                Nothing    -> Sub (simplify vrs (Var e1)) (simplify vrs (Var e2))
  simplify vrs (Sub (Var e1) (Const e2))   = case Map.lookup e1 vrs of
                                  Just n   -> Const (eval vrs (Sub (Var e1) (Const e2)))
                                  Nothing  -> Sub (Var e1) (Const e2)
  simplify vrs (Sub (Const e1) (Var e2))   = case Map.lookup e2 vrs of
                                           Just y -> Const (eval vrs (Sub (Var e2) (Const e1)))
                                           Nothing -> Sub (Const e1) (Var e2) 
  simplify vrs (Sub e1 e2)                 = Sub (simplify vrs e1) (simplify vrs e2)

  simplify vrs (Sin (Const a))             = case (eval vrs (Sin (Const a))) of
                                  Answer n -> Const n
                                  Error e  -> Sin (Const a)
  simplify vrs (Sin (Var e))               = case (Map.lookup e vrs) of
                                  Just x   -> Const (eval vrs (Sin (Var e)))
                                  Nothing  -> Sin (simplify vrs (Var e))
  simplify vrs (Sin x)                     = Sin x -- (Sin x) will remain as is, no further simplification can be done

  simplify vrs (Cos (Const a))             = case (eval vrs (Cos (Const a))) of
                                  Answer n -> Const n
                                  Error e  -> Cos (Const a)
  simplify vrs (Cos (Var x))               = case (Map.lookup x vrs) of
                                  Just t   -> Const (eval vrs (Cos (Var x)))
                                  Nothing  -> Cos (simplify vrs (Var x))
  simplify vrs (Cos x)                     = Cos x -- (Cos x) will remain as is, no further simplification can be done

  simplify vrs (Log e (Const 1))           = Const 0
  simplify vrs (Log e (Const a))           = case (eval vrs (Log e (Const a))) of
                                  Answer x -> Const x
                                  Error i  -> Log e
  simplify vrs (Log e (Var x))             = Log e (Var x)
  simplify vrs (Log e (Exp x y))           = simplify vrs $ Multi y (Log e x)

  simplify vrs (Exp 0 (Const _))           = Const 0 -- 0 to the power of anything is just zero
  simplify vrs (Exp _ (Const 0))           = Const 1 -- Anything to the power of zero is just 1
  simplify vrs (Exp (Const a) (Const b))   = Const (a ** b) -- This is regular exponentiation of a to the power of b
  simplify vrs (Exp (E e) (Const 1))       = e -- Because natural 'e' to the power of 1 is just 'e'
  simplify vrs (Exp (Var a) (Var b))       = case (Map.lookup a vrs) of
                                Just m     -> case (Map.lookup b vrs) of
                                  Just n   -> Const (eval vrs (Exp (Var a) (Var b)))
                                  Nothing  -> Exp (simplify vrs (Var a)) (simplify vrs (Var b))
                                Nothing    -> Exp (simplify vrs (Var a)) (simplify vrs (Var b)) 
  simplify vrs (Exp x y)                   = Exp (simplify vrs x) (simplify vrs y)

  simplify vrs (Var x)                     = case Map.lookup x vrs of
                                  Just v   -> Const (eval vrs (Var x))
                                  Nothing  -> Var x 

  simplify vrs (Const a)                   = Const a -- Constants can't really be simplified 
  simplify vrs (Multi (Const 0) _ )        = Const 0 -- Zero times anything is zero
  simplify vrs (Multi _ (Const 0))         = Const 0 -- Anything times zero is 0
  simplify vrs (Multi (Const 1) _ )        = simplify vrs e1 -- One times anything is anything
  simplify vrs (Multi _ (Const 1))         = simplify vrs e1 -- Anything times one is anything          
  simplify vrs (Multi (Const a) (Const b)) = Const (a * b) -- This is regular binary multiplication
  simplify vrs (Multi (Var a) (Var b))     = case (Map.lookup a vrs) of
                                Just m     -> case (Map.lookup y vrs) of
                                  Just n   -> Const (eval vrs (Multi (Var a) (Var b)))
                                  Nothing  -> Multi (simplify vrs (Var a)) (simplify vrs (Var b))
                                Nothing    -> Multi (simplify vrs (Var a)) (simplify vrs (Var b))
  simplify vrs (Multi (Var a) (Const b))   = case Map.lookup a vrs of
                                  Just m   -> Const (eval vrs (Multi (Var a) (Const b)))
                                  Nothing  -> Multi (Var a) (Const b)
  simplify vrs (Multi (Const a) (Var b))   = case Map.lookup b vrs of
                                  Just m   -> Const (eval vrs (Multi (Var b) (Const a)))
                                  Nothing  -> Multi (Const a) (Var b) 
  simplify vrs (Multi x y)                 = Multi (simplify vrs x) (simplify vrs y) -- Anything else will be either be returned as is, or an attempt to simplify will be made

{- 
Ain't nobody got them for dem instances. We got examzz n shizz
-}



