module Data
(Token(..)
, Operator(..)
, Var(..)
, Compute(..)
, isRat
, isIma
, isMat
, isFct
, isOp
, isNotOp
, isVar
, isNumb
, isNumbNotNull
, isMatricialMult
, isCUnknown
, showTkList
, showTkList0
, showMat
, showVar
, showVarMap
, putVar
, showTkListName
, putVarLn) where

import Data.HashMap.Strict as Hm (HashMap, foldl')
import Data.List

data Var = Rat !String !Float | Ima !String ![Token] | Mat !String ![[Float]] | Fct !String !String ![Token] | Void
instance Show (Var) where
    show (Rat name value) = name ++ " = " ++ show value
    show (Ima name value) = name ++ " = " ++ showTkListName "i" value
    show (Mat name tab) = name ++ " = " ++ show tab
    show (Fct name var value) = name ++ "(" ++ var ++ ") = " ++ showTkListName var value
    show Void = "Error Input"

data Token = Op !Operator | Numb !Float !Int | Var !String | UnParsed deriving (Eq)
instance Show (Token) where
    show (Op x) = show x
    show (Numb x 0) = show x
    show (Numb x y) = show x ++ "X^" ++ show y
    show (Var x) = x
    show UnParsed = "Error"

data Operator = Add | Minus | Mult | Div | Pow | Mod | MatricialMult | Equal | OpenBracket | CloseBracket deriving (Eq)
instance Show (Operator) where
    show Add = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Pow = "^"
    show Mod = "%"
    show MatricialMult = "**"
    show Equal = "="
    show OpenBracket = "("
    show CloseBracket = ")"

data Compute = COp !Operator | CTk !Token | CVar !Var | CUnknown !String
instance Show (Compute) where
    show (COp x) = show x
    show (CTk x) = show x
    show (CVar x) = drop 2 $ showVar x
    show (CUnknown x) = show x

showTkList :: [Token] -> String
showTkList = showTkListName "x"

showTkList0 :: [Token] -> String
showTkList0 x = showTkList x ++ "= 0.0"

showTkListName :: String -> [Token] -> String
showTkListName _ [] = []
showTkListName name ((Numb x y):xs)
    | x == 0 = "0 " ++ showTkListName name xs
    | x == 1 && y == 1 = name ++ " " ++ showTkListName name xs
    | x == (-1) && y == 1 = "-" ++ name ++ " " ++ showTkListName name xs
    | x == 1 && y /= 0 = name ++ "^" ++ show y ++ " " ++ showTkListName name xs
    | x == (-1) && y /= 0 = "-" ++ name ++ "^" ++ show y ++ " " ++ showTkListName name xs
    | y == 1 = show x ++ name ++ " " ++ showTkListName name xs
    | y /= 0 = show x ++ name ++ "^" ++ show y ++ " " ++ showTkListName name xs
    | otherwise = show x ++ " " ++ showTkListName name xs
showTkListName name (x:xs) = show x ++ " " ++ showTkListName name xs

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
showVar (Ima _ v) = "  " ++ showTkListName "i" v 
showVar (Mat _ v) = showMat v
showVar (Fct _ var v) = "  " ++ showTkListName var v
showVar x = show x

isRat :: Var -> Bool
isRat (Rat _ _) = True
isRat _ = False

isIma :: Var -> Bool
isIma (Ima _ _) = True
isIma _ = False

isMat :: Var -> Bool
isMat (Mat _ _) = True
isMat _ = False

isFct :: Var -> Bool
isFct (Fct _ _ _) = True
isFct _ = False

isOp :: Token -> Bool
isOp (Op _) = True
isOp _ = False

isNotOp = not . isOp

isVar :: Token -> Bool
isVar (Var _) = True
isVar _ = False

isNumb :: Token -> Bool
isNumb (Numb _ _) = True
isNumb _ = False

isNumbNotNull :: Token -> Bool
isNumbNotNull (Numb x _)
    | x == 0 = False
    | otherwise = True
isNumbNotNull _ = False

isMatricialMult :: Token -> Bool
isMatricialMult (Op MatricialMult) = True
isMatricialMult _ = False

isCUnknown :: Compute -> Bool
isCUnknown (CUnknown _) = True
isCUnknown x = False

showVarMap :: HashMap String Var -> String
showVarMap hm =
    let
        rat = "  Rationals:\n" ++ (foldl (++) "" $ sort $ [ "  - " ++ show x ++ "\n" | x <- all, isRat x])
        ima = "  Imaginary:\n" ++ (foldl (++) "" $ sort $ [ "  - " ++ show x ++ "\n" | x <- all, isIma x])
        mat = "  Matrix:\n" ++ (foldl (++) "" $ sort $ [ "  - " ++ show x ++ "\n" | x <- all, isMat x])
        fct = "  Functions:\n" ++ (foldl (++) "" $ sort $ [ "  - " ++ show x ++ "\n" | x <- all, isFct x])
    in rat ++ ima ++ mat ++ fct
    where all = Hm.foldl' (\acc x -> x : acc) [] hm