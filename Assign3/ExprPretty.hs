module ExprPretty where

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

parens2 :: String -> String
parens2 ss = "\"" ++ ss ++ "\""

instance Show a => Show (Expr a) where
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Sin e)      = " Sin " ++ parens (show e)
  show (Cos e)      = " Cos " ++ parens (show e)
  show (Log a e)    = " Log Base " ++ (show a) ++ " Of " ++ parens (show e)
  show (Exp e)      = " (Natural) e ^ " ++ parens (show e)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
