module Data
(Token(..)
, Operator(..)
, Var(..)
, showTkList
, showMat
, showVar
, putVar
, putVarLn) where

data Var = Rat !String !Float | Ima !String ![Token] | Mat !String ![[Float]] | Fct !String !String ![Token] | Void
instance Show (Var) where
    show (Rat name value) = name ++ " = " ++ show value
    show (Ima name value) = name ++ " = " ++ showTkList value
    show (Mat name tab) = name ++ " = " ++ show tab
    show (Fct name var value) = name ++ "(" ++ var ++ ") = " ++ showTkList value
    show Void = "Error Input"

data Token = Op !Operator | Numb !Float !Int | Var !String | UnParsed deriving (Eq)
instance Show (Token) where
    show (Op x) = show x
    show (Numb x 0) = show x
    show (Numb x y) = show x ++ "X^" ++ show y
    show (Var x) = x
    show UnParsed = "Error"

data Operator = Add | Minus | Mult | Div | Pow | Equal | OpenBracket | CloseBracket deriving (Eq)
instance Show (Operator) where
    show Add = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Pow = "^"
    show Equal = "="
    show OpenBracket = "("
    show CloseBracket = ")"

showTkList :: [Token] -> String
showTkList [] = "0.0"
showTkList (x:[]) = show x
showTkList (x:xs) = show x ++ " " ++ showTkList xs

showMat :: [[Float]] -> String
showMat [] = []
showMat (x:[]) = "  " ++ show x
showMat (x:xs) = "  " ++ show x ++ "\n" ++ showMat xs

putVar :: Var -> IO ()
putVar = putStr . showVar

putVarLn :: Var -> IO ()
putVarLn = putStrLn . showVar

showVar :: Var -> String
showVar (Rat _ v) = "  " ++ show v
showVar (Ima _ v) = "  " ++ showTkList v
showVar (Mat _ v) = showMat v
showVar (Fct _ _ v) = "  " ++ showTkList v
showVar x = show x