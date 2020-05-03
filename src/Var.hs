module Var (toVar, checkType, getName) where

import Token
import Parsing
import Data
import Data.Char
import Data.List
import Debug.Trace
import Data.HashMap.Strict as Hm (HashMap, member, (!))
import Bracket
import Polish

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
        newLst = toBasictoken hm lst ""

{-- IMAGINARY --}

checkIma :: [Token] -> HashMap String Var -> IO Var
checkIma lst hm
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | powerI newLst = do
        putStrLn "Error: powered i is invalid"
        return Void
    | otherwise = return (Ima "" []) 
    where
        newLst = toBasictoken hm lst "i"
        powerI :: [Token] -> Bool
        powerI [] = False
        powerI ((Numb _ b):(Op Pow):xs)
            | b /= 0 = True
        powerI (x:xs) = powerI xs

{-- MANAGER --}

varToNumb :: HashMap String Var -> String -> Token -> Token 
varToNumb hm except x@(Var name)
    | name == except = (Numb 1 1)
    | member name hm = case hm ! name of
        (Rat _ value) -> (Numb value 0)
        _ -> x 
varToNumb _ _ x  = x 

toBasictoken :: HashMap String Var -> [Token] -> String -> [Token]
toBasictoken hm tk except = 
    let maped = map (varToNumb hm except) tk
        ret = case maped of
            ((Op Minus):x1:xs) -> appMinus x1 : xs
            _ -> maped
    in toBasictoken2 ret
    where
        toBasictoken2 :: [Token] -> [Token]
        toBasictoken2 [] = []
        toBasictoken2 (x@(Op _):(Op Minus):y@(Numb _ _):xs) = x : appMinus y : toBasictoken2 xs
        toBasictoken2 (x@(Numb _ _):y@(Numb _ _):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x@(Op CloseBracket):y@(Numb _ _):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x@(Numb _ _):y@(Op OpenBracket):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x:xs) = x : toBasictoken2 xs

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
            | (Var "i") `elem` lst =
                let tmp = solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst "i"
                in Ima name tmp
            | (Var "[") `elem` lst = Mat name (toMat lst hm)
            | otherwise =
                let tmp = simpleReduce $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst ""
                in Rat name tmp
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
            | (Var "i") `elem` lst = checkIma lst hm
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