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

  -- | Simplifies an expression and substitutes the supplied values
  simplify :: Map.Map String a -> Expr a -> Expr a

  -- | Performs partial differentiation w.r.t identifier
  partDiff :: String -> Expr a -> Expr a

  {- The following functions are implementations of the in-built functions: +, -, * ^ . They are able to operate over the 'Expr' data type, where the in-built functions are not. Hence, these are better. -}
  (!+) :: Expr a -> Expr a -> Expr a -- This is the type signature for Adding. It takes TWO 'Expr' a, and returns an 'Expr' a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2 {- For Example: (Const 410) !+ (Const 10) = Const (420.0) : Everything is returned as a float -}

  (!-) :: Expr a -> Expr a -> Expr a -- This is the type signature for Subtracting. It takes TWO 'Expr' a, and returns an 'Expr' a
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2 {- For Example: (Const 430) !- (Const 10) = Const (420.0) : Everything is returned as a float -}
  
  (!*) :: Expr a -> Expr a -> Expr a -- This is the type signature for Multiplying. It takes TWO 'Expr' a, and returns an 'Expr' a
  e1 !* e2 = simplify (Map.fromList []) $ Multi e1 e2 {- For Example: (Const 4.2) !* (Const 10) = Const (420.0) : Everything is returned as a float -}
  
  (!^) :: Expr a -> Expr a -> Expr a -- This is the type signature for Exponentiation. It takes TWO 'Expr' a, and returns an 'Expr' a
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2 {- For Example: (Const 2) !^ (Const 10) = Const (1024.0) : Everything is returned as a float -}

  {- | This function will convert an ordinary number into the 'Expr' data type, by wrapping it in 'Const' -}
  val :: a       -- ^ The ordinary number you wanna convert
      -> Expr a  -- ^ The resulting answer wrapped in 'Expr'
  -- The type signature states that it takes an 'a' and returns an 'Expr' wrapping an 'a'
  val x = Const x {- For Example: It'll accept 420 and return (Const 420.0), without the brackets of course -}
  
  {- | This function will convert an ordinary string into the 'Expr' data type, by wrapping it in 'Var' -}
  var :: String -- ^ The String you want to wrap
      -> Expr a -- ^ The result wrapped in 'Expr'
  -- The type signature states that it takes an 'a' and returns an 'a' wrapped in 'Expr'
  var x = Var x {- For Example: It'll accept "pls_gib_A+" and return (Var "pls_gib_A+"), without the brackets of course -}
  
  {- 
  The following four functions (myCos, mySin, myLog, & myE) will convert 'Const' numbers into Cos, Sin, Log, or E, respectively. All 
  funtions take an 'Expr' a and return another 'Expr' a. Here are a few examples of how to use them:
  (1) myCos (Const 1.0)              --> Cos (Const 1.0)
  (2) mySin (Const 0.0)              --> Sin (Const 0.0)
  (3) myLog (Const 100.0)            --> Log (Const 100.0)
  (4) myE (Const 2.718)              --> E (Const 2.718) 
  -}

  -- See (1) Above
  -- Converts from 'Const' to 'Cos'
  myCos :: Expr a -- ^ The 'Const' you want to convert
        -> Expr a -- ^ Returned as 'Cos'
  myCos x = Cos x 
  
  -- See (2) Above
  -- Converts from 'Const' to 'Sin'
  mySin :: Expr a -- ^ The 'Const' you want to convert
        -> Expr a -- ^ Returned as 'Sin'
  mySin x = Sin x
  
  -- See (3) Above
  -- Converts from 'Const' to 'Log'
  myLog :: Expr a -- ^ The 'Const' you want to convert
        -> Expr a -- ^ Returned as 'Log'
  myLog x = Log x
  
  -- See (4) Above
  -- Converts from 'Const' to 'E'
  myE :: Expr a -- ^ The 'Const' you want to convert
      -> Expr a -- ^ Returned as 'E'
  myE e = E e
  
  {- 
  The following four functions (mySuperCos, mySuperSin, mySuperLog, & mySuperE) will convert ordinary numbers into Cos, Sin, Log, or E, respectively. 
  All funtions take an 'a' and return 'a' wrapped in 'Expr'. Even though they are not really required, I thought I'd add him.
  Here are a few examples of how to use them:
  (5) mySuperCos 1   --> Cos (Const 1.0)
  (6) mySuperSin 0   --> Sin (Const 0.0)
  (7) mySuperLog 100 --> Log (Const 100.0)
  (8) mySuperE 2.718 --> E (Const 2.718) 
  -}

  -- See (5) Above
  -- ^ Wraps a number in the 'Cos' datatype
  mySuperCos :: a      -- ^ The ordinary number you want to convert to 'Cos'
             -> Expr a -- ^ The answer wrapped in 'Cos'
  mySuperCos x = Cos x

  -- See (6) Above
  -- ^ Wraps a number in the 'Sin' datatype
  mySuperSin :: a         -- ^ The ordinary number you want to convert to 'Sin'
                -> Expr a -- ^ The answer wrapped in 'Sin'
  mySuperSin x = Sin x

  -- See (7) Above
  -- ^ Wraps a number in the 'Log' datatype
  mySuperLog :: a      -- ^ The ordinary number you want to convert to 'Log'
             -> Expr a -- ^ The answer wrapped in 'Log'
  mySuperLog x = Log x

  -- See (8) Above
  -- ^ Wraps a number in the 'E' datatype
  mySuperE :: a         -- ^ The ordinary number you want to convert to 'E'
              -> Expr a -- ^ The answer wrapped in 'E'
  mySuperE e = E e

  -- | The 'Answer' datatype stores the value of an expression based on the final answer
  data Answer a = Error String -- ^ Wraps a 'String' in 'Error' if something goes wrong with the expression
                | Answer a     -- ^ Wraps the right answer in 'Answer' if the expression is computed
    deriving (Show)

-- * Evaluating Expressions

{-This defines the "rules" for eval, partDiff, and simplify-}
{- | This will evaluate, simplify, and differentiate expressions. 
It will evaluate fully, simplify as much as possible, and differentiate partially or fully, depending on the expression.
Some expressions cannot be further simplified
-}
instance (Floating a) => DiffExpr a where
  
  {- $ THIS IS THE EVALUATION FOR EXPR $ -}
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

  {- $ THIS IS HOW EXPR WILL BE DIFFERENTIATED (PARTIALLY OR FULLY) $ -}
  partDiff p (E e)                         = Multi (E e) (partDiff p e) -- Differentiates Natural E
  partDiff p (Add e1 e2)                   = Add (partDiff p e1) (partDiff p e2) -- Rules For Differentiating Binary Addition
  partDiff p (Sub e1 e2)                   = Sub (partDiff p e1) (partDiff p e2) -- Rules For Differentiating Binary Subtraction
  partDiff p (Sin x)                       = Multi (Cos x) (partDiff p x) -- Differentiates Sin
  partDiff p (Cos x)                       = Multi (Neg (Sin x)) (partDiff p x) -- Differentiates Cosine
  partDiff p (Log a b)                     = Multi (Div (Const 1) (Ln (Const a))) (partDiff p b) -- Differentiates Logarithms
  partDiff p (Exp a b)                     = Mult (Mult b (Exp a (Add b (Const (-1))))) (partDiff p a) -- Differentiates Exponents
  partDiff p (Var e)                       = if e == p then (Const 1) else (Const 0) -- Differentiates variables
  partDiff _ (Const _)                     = Const 0 -- All Constants Differentiate To 0
  partDiff s (Multi e1 e2)                 = Add (Multi (partDiff s e1) e2) (Multi e1 (partDiff s e2)) -- Rules For Differentiating Binary Multiplication


  {- $ THIS IS HOW EXPR WILL BE SIMPLIFIED $ -}

  -- Rules For Natural Exponent E
  simplify vrs (E (Const 0) e)             = Const 1 -- E to the power of 0 is 1
  simplify vrs (E (Const a) e)             = case (eval vrs (E Const a))) of -- E to the power of a number
                                Answer n   -> Const n
                                Error e    -> E (Const a)
  simplify vrs (E (Var x) e)               = E (simplify vrs e) -- E to the power of a variable (i.e. e ^ n)
  simplify vrs (E (Log x) e)               = x -- E to the power of log x is just x
  simplify vrs (E (Multi x (Log y)) e)     = Pow x y 
  simplify vrs (E (x) e)                   = x -- Anything else not mentioned won't simplify
  
  -- Rules For Binary Addition
  simplify vrs (Add (Const 0) e)           = simplify vrs e -- Adding 0 to 'e' is just 'e'
  simplify vrs (Add e (Const 0))           = simplify vrs e -- ^^ See Above ^^
  simplify vrs (Add (Const e1) (Const e2)) = Const (e1 + e2) -- Adding two numbers together is simple addition
  simplify vrs (Add (Var e1) (Var e2))     = case (Map.lookup x vrs) of -- Adding two variables
                                Just n     -> case (Map.lookup e2 vrs) of
                                  Just m   -> Const (eval vrs (Add (Var e1) (Var e2)))
                                  Nothing  -> Add (simplify vrs (Var e1)) (simplify vrs (Var e2))
                                Nothing    -> Add (simplify vrs (Var e1)) (simplify vrs (Var e2))
  simplify vrs (Add (Var e1) (Const e2))   = case Map.lookup e1 vrs of -- Adding a variable and a constant is the expression itself
                                  Just n   -> Const (eval vrs (Add (Var e1) (Const e2)))
                                  Nothing  -> Add (Var e1) (Const e2)
  simplify vrs (Add (Const e1) (Var e2))   = case Map.lookup e2 vrs of -- Same As Above
                                  Just y   -> Const (eval vrs (Add (Var e2) (Const e1)))
                                  Nothing  -> Add (Const e1) (Var e2) 
  simplify vrs (Add e1 e2)                 = Add (simplify vrs e1) (simplify vrs e2)

  -- Rules For Binary Subtraction
  simplify vrs (Sub (Const 0) e)           = simplify vrs e -- Subtracting 0 from 'e' is just 'e'
  simplify vrs (Sub e (Const 0))           = simplify vrs e -- Same as above
  simplify vrs (Sub (Const e1) (Const e2)) = Const (e1 - e2) -- Subtracting two constants is simple binary subtraction
  simplify vrs (Sub (Var e1) (Var e2))     = case (Map.lookup x vrs) of -- Subtracting two variables
                                Just n     -> case (Map.lookup e2 vrs) of
                                  Just m   -> Const (eval vrs (Sub (Var e1) (Var e2)))
                                  Nothing  -> Sub (simplify vrs (Var e1)) (simplify vrs (Var e2))
                                Nothing    -> Sub (simplify vrs (Var e1)) (simplify vrs (Var e2))
  simplify vrs (Sub (Var e1) (Const e2))   = case Map.lookup e1 vrs of -- Subtracting a variable and a constant
                                  Just n   -> Const (eval vrs (Sub (Var e1) (Const e2)))
                                  Nothing  -> Sub (Var e1) (Const e2)
  simplify vrs (Sub (Const e1) (Var e2))   = case Map.lookup e2 vrs of -- Same as above
                                           Just y -> Const (eval vrs (Sub (Var e2) (Const e1)))
                                           Nothing -> Sub (Const e1) (Var e2) 
  simplify vrs (Sub e1 e2)                 = Sub (simplify vrs e1) (simplify vrs e2)


  -- Rules For Trignometric Sine
  simplify vrs (Sin (Const a))             = case (eval vrs (Sin (Const a))) of -- Sine of a constant
                                  Answer n -> Const n
                                  Error e  -> Sin (Const a)
  simplify vrs (Sin (Var e))               = case (Map.lookup e vrs) of -- Sine of a variable
                                  Just x   -> Const (eval vrs (Sin (Var e)))
                                  Nothing  -> Sin (simplify vrs (Var e))
  simplify vrs (Sin s)                     = Sin s -- (Sin s) will remain as is, probably because no further simplification can be done

  -- Rules For Trignometric Cosine
  simplify vrs (Cos (Const a))             = case (eval vrs (Cos (Const a))) of -- Cosine of a constant
                                  Answer n -> Const n
                                  Error e  -> Cos (Const a)
  simplify vrs (Cos (Var x))               = case (Map.lookup x vrs) of -- Cosine of a variable
                                  Just t   -> Const (eval vrs (Cos (Var x)))
                                  Nothing  -> Cos (simplify vrs (Var x))
  simplify vrs (Cos c)                     = Cos c -- (Cos c) will remain as is, probably because no further simplification can be done


  -- Rules For Logarithms
  simplify vrs (Log l (Const 1.0))         = Const 0 -- Log of 1.0
  simplify vrs (Log l (Const a))           = case (eval vrs (Log l (Const a))) of -- Log of a constant 
                                  Answer x -> Const x
                                  Error i  -> Log l
  simplify vrs (Log l (Var x))             = Log l (Var x) -- Log of a variable
  simplify vrs (Log l (Exp x y))           = simplify vrs (Multi y (Log l x)) -- Log of exponents

  -- Rules For Exponentiation
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

  -- Rule For Constants
  simplify vrs (Const a)                   = Const a -- Constants can't really be simplified 
  
  -- Rules For Multiplication
  simplify vrs (Multi (Const 0) _ )        = Const 0 -- Zero times anything is zero
  simplify vrs (Multi _ (Const 0))         = Const 0 -- Anything times zero is 0
  simplify vrs (Multi (Const 1) _ )        = simplify vrs e1 -- One times anything is anything
  simplify vrs (Multi _ (Const 1))         = simplify vrs e1 -- Anything times one is anything          
  simplify vrs (Multi (Const a) (Const b)) = Const (a * b) -- This is regular binary multiplication
  simplify vrs (Multi (Var a) (Var b))     = case (Map.lookup a vrs) of -- Multiplying two variables
                                Just m     -> case (Map.lookup y vrs) of
                                  Just n   -> Const (eval vrs (Multi (Var a) (Var b)))
                                  Nothing  -> Multi (simplify vrs (Var a)) (simplify vrs (Var b))
                                Nothing    -> Multi (simplify vrs (Var a)) (simplify vrs (Var b))
  simplify vrs (Multi (Var a) (Const b))   = case Map.lookup a vrs of -- Multiplying a variable and a constant
                                  Just m   -> Const (eval vrs (Multi (Var a) (Const b)))
                                  Nothing  -> Multi (Var a) (Const b)
  simplify vrs (Multi (Const a) (Var b))   = case Map.lookup b vrs of -- Multiplying a constant and a variable
                                  Just m   -> Const (eval vrs (Multi (Var b) (Const a)))
                                  Nothing  -> Multi (Const a) (Var b) 
  simplify vrs (Multi x y)                 = Multi (simplify vrs x) (simplify vrs y) -- Anything else will be either be returned as is, or an attempt to simplify will be made

{- 
Ain't nobody got them for dem instances. 
We got examzz n shizz. 
I'll let GHCi do all the work.
-}



