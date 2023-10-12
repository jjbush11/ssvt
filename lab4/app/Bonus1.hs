{-# LANGUAGE InstanceSigs #-}

module Bonus1 where

import Data.List (intercalate)
import Lecture6
import Test.QuickCheck

instance Show Expr where
  show :: Expr -> String
  show e = case e of
    I i -> show i
    V v -> v
    Add e1 e2 -> show e1 ++ " - " ++ show e2
    Subtr e1 e2 -> show e1 ++ " - " ++ show e2
    Mult e1 e2 -> show e1 ++ " * " ++ show e2

instance Show Condition where
  show :: Condition -> String
  show c = case c of
    Prp v -> v
    Eq e1 e2 -> show e1 ++ " == " ++ show e2
    Lt e1 e2 -> show e1 ++ " < " ++ show e2
    Gt e1 e2 -> show e1 ++ " > " ++ show e2
    Ng c -> "!(" ++ show c ++ ")"
    Cj cs -> intercalate " && " (map show cs)
    Dj cs -> intercalate " || " (map show cs)

-- A function to add indentation
indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- A show instance for the Statement data type.
instance Show Statement where
  show :: Statement -> String
  show s = case s of
    Ass v e -> v ++ " = " ++ show e ++ ";"
    Cond c s1 s2 ->
      "if ("
        ++ show c
        ++ ") {\n"
        ++ indent (show s1)
        ++ "}\nelse {\n"
        ++ indent (show s2)
        ++ "}"
    Seq stmts ->
      intercalate "" (map (indent . show) stmts)
    While c s ->
      "while ("
        ++ show c
        ++ ") {\n"
        ++ indent (show s)
        ++ "}"

main :: IO ()
main = do
  print fib
  print fib'