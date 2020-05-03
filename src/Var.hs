module Var (toVar, checkType, getName) where

import Token
import Parsing
import Data
import Data.Char
import Data.List
import Debug.Trace
import Data.HashMap.Strict as Hm (HashMap, member, (!))

{-- MATRICE --}

checkMat :: [Token] -> HashMap String Var -> IO Var
checkMat lst hm
    | checkOp newLst =  do
        putStrLn $ "Error: Non expected operator"
        return Void
    | checkVar newLst /= [] = do
        putStrLn $ "Error: Var \"" ++ checkVar newLst ++ "\" in matrice" 
        return Void
    | checkComma newLst = do
        putStrLn "Error: Missmatched Comma"
        return Void
    | checkBracket newLst 0 = do
        putStrLn "Error: Missmatched Bracket"
        return Void
    | (checkNumber $ toMat newLst hm) = do
        putStrLn "Error: Not same number of element in matrice"
        return Void
    | otherwise = return (Mat "" [])
    where
        newLst = toMatToken hm lst
        checkVar :: [Token] -> String
        checkVar [] = []
        checkVar ((Var name):_)
            | name /= ";" && name /= "," && name /= "[" && name /= "]" = name
        checkVar (x:xs) = checkVar xs
        checkOp :: [Token] -> Bool
        checkOp [] = False
        checkOp ((Op _):_) = True
        checkOp (x:xs) = checkOp xs
        checkComma :: [Token] -> Bool
        checkComma [] = False
        checkComma (x@(Numb _ _):y@(Var ","):z@(Numb _ _):xs) = checkComma (z : xs)
        checkComma (x@(Var "]"):y@(Var ";"):z@(Var "["):xs) = checkComma xs
        checkComma ((Var "]"):(Var "["):xs) = True
        checkComma ((Numb _ _):(Numb _ _):xs) = True
        checkComma ((Var ","):_) = True
        checkComma ((Var ";"):_) = True
        checkComma (x:xs) = checkComma xs
        checkBracket :: [Token] -> Int -> Bool
        checkBracket [] 0 = False
        checkBracket [] _ = True
        checkBracket _ n
            | n > 2 = True
            | n < 0 = True
        checkBracket ((Var "["):xs) n = checkBracket xs (n + 1)
        checkBracket ((Var "]"):xs) n = checkBracket xs (n - 1)
        checkBracket (_:xs) n = checkBracket xs n 
        checkNumber :: [[Float]] -> Bool
        checkNumber [] = True
        checkNumber lst = 
            let size = length $ head lst
            in not (all (\y -> length y == size) lst)

toMatToken :: HashMap String Var -> [Token] -> [Token]
toMatToken _ [] = []
toMatToken hm ((Op Minus):x@(Numb _ _):xs) = appMinus x : toMatToken hm xs
toMatToken hm ((Op Minus):x@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> appMinus (Numb value 0) : toMatToken hm xs
        _ -> x : toMatToken hm xs
toMatToken hm (x@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> (Numb value 0) : toMatToken hm xs
        _ -> x : toMatToken hm xs
toMatToken hm (x:xs) = x : toMatToken hm xs

toMat :: [Token] -> HashMap String Var -> [[Float]]
toMat [] _ = []
toMat x hm = read $ showTkList $ toMatToken hm $ map toComma x
    where
        toComma :: Token -> Token
        toComma (Var ";") = Var ","
        toComma x = x

{-- RATIONAL --}

checkRat :: [Token] -> HashMap String Var -> IO Var
checkRat lst hm
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | otherwise = return (Rat "" 0) 
    where
        newLst = toRatToken hm lst

toRatToken :: HashMap String Var -> [Token] -> [Token]
toRatToken _ [] = []
toRatToken hm (x@(Op _):(Op Minus):y@(Numb _ _):xs) = x : appMinus y : toRatToken hm xs
toRatToken hm (x@(Op _):y@(Op Minus):z@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> x : appMinus (Numb value 0) : toRatToken hm xs
        _ -> x : y : z : toRatToken hm xs
toRatToken hm (x@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> (Numb value 0) : toRatToken hm xs
        _ -> x : toRatToken hm xs
toRatToken hm (x:y@(Op Minus):z:xs) = x : y : z : toRatToken hm xs
toRatToken hm ((Op Minus):x@(Numb _ _):xs) = appMinus x : toRatToken hm xs
toRatToken hm (x@(Op Minus):y@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> appMinus (Numb value 0) : toRatToken hm xs
        _ -> x : y : toRatToken hm xs
toRatToken hm (x@(Numb _ _):y@(Var name):xs)
    | member name hm = case hm ! name of
        (Rat _ value) -> x : (Op Mult) : (Numb value 0) : toRatToken hm xs
        _ -> x : y : toRatToken hm xs
toRatToken hm (x@(Op _):xs) = x : toRatToken hm xs
toRatToken hm (x:xs) = x : toRatToken hm xs

{-- IMAGINARY --}

form :: [Token] -> [Token]
form [] = []
form (x@(Numb _ _):y@(Var _):xs) = x : Op Mult : form (y : xs) 
form (x@(Var _):y@(Var _):xs) = x : Op Mult : form (y : xs)
form (x:xs) = x : form xs

toVar :: [Token] -> HashMap String Var -> Var
toVar lst hm = toVar2 (takeWhile (/= (Op Equal)) lst) (tail $ dropWhile (/= (Op Equal)) lst) hm
    where
        toVar2 :: [Token] -> [Token] -> HashMap String Var -> Var
        toVar2 ((Var name):[]) lst hm
            -- | (Var "i") `elem` lst = Ima name (toIma $ formIma lst)
            | (Var "[") `elem` lst = Mat name (toMat lst hm)
            | otherwise = Rat name (simpleReduce $ toRatToken hm lst)
        toVar2 ((Var name):(Op OpenBracket):(Var var):(Op CloseBracket):[]) lst _ = Fct name var (form lst)
        toVar2 _ _ _ = Void

checkType :: [Token] -> HashMap String Var -> IO Var
checkType lst hm = checkType2 (takeWhile (/= (Op Equal)) lst) (tail $ dropWhile (/= (Op Equal)) lst) hm
    where
        checkType2 :: [Token] -> [Token] -> HashMap String Var -> IO Var
        checkType2 ((Var name):[]) lst hm
            | any (\x -> not $ isLetter x) name || name == "i" = do
                putStrLn "Wrong var name"
                return (Void)
            -- | (Var "i") `elem` lst = Ima name (toIma $ formIma lst)
            | (Var "[") `elem` lst = checkMat lst hm
            | otherwise = checkRat lst hm
        checkType2 ((Var name):(Op OpenBracket):(Var var):(Op CloseBracket):[]) lst hm = return (Fct name var (form lst))
        checkType2 _ _ _ = do
            putStrLn "Error: No founded patern"
            return (Void)

getName :: Var -> String
getName (Rat name _) = name
getName (Ima name _) = name
getName (Mat name _) = name
getName (Fct name _ _ ) = name
getName _ = "No name Found"