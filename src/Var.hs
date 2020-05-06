module Var (toVar, checkType, getName, deployVar, endOfMat, toMat) where

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

checkMat :: [Token] -> HashMap String Var -> String -> IO Var
checkMat lst hm name
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
    | otherwise = return (Mat name [])
    where
        newLst = toMatToken hm lst
        checkVar :: [Token] -> String
        checkVar [] = []
        checkVar ((Var name):_)
            | name /= ";" && name /= "," && name /= "[" && name /= "]" = name
        checkVar (x:xs) = checkVar xs
        checkOp :: [Token] -> Bool
        checkOp = any isOp
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

endOfMat :: [Token] -> Int -> Int
endOfMat [] _ = 0
endOfMat ((Var "["):xs) br = 1 + endOfMat xs (br + 1)
endOfMat ((Var "]"):xs) br
    | br == 1 = 1
    | otherwise = 1 + endOfMat xs (br - 1)
endOfMat (x:xs) br = 1 + endOfMat xs br

toMat :: [Token] -> HashMap String Var -> [[Float]]
toMat [] _ = []
toMat x hm = read $ showTkList $ toMatToken hm $ map toComma x
    where
        toComma :: Token -> Token
        toComma (Var ";") = Var ","
        toComma x = x

{-- RATIONAL --}

checkRat :: [Token] -> HashMap String Var -> String -> IO Var
checkRat lst hm name
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | otherwise = return (Rat name 0) 
    where
        newLst = toBasictoken hm lst ""

toRat :: [Token] -> HashMap String Var -> Float
toRat lst hm = simpleReduce $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst ""

{-- IMAGINARY --}

checkIma :: [Token] -> HashMap String Var -> String -> IO Var
checkIma lst hm name
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | powerI newLst = do
        putStrLn "Error: powered i is invalid"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | otherwise = return (Ima name []) 
    where
        newLst = toBasictoken hm lst "i"
        powerI :: [Token] -> Bool
        powerI [] = False
        powerI ((Numb _ b):(Op Pow):xs)
            | b /= 0 = True
        powerI (x:xs) = powerI xs

toIma :: [Token] -> HashMap String Var -> [Token]
toIma lst hm = makeItRedableRev $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst "i"

{-- FUNCTION --}

checkFunction :: String -> [Token] -> HashMap String Var -> String -> IO Var
checkFunction var lst hm name
    | var == "i" = do
        putStrLn "Wrong var name"
        return Void
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | otherwise = return (Fct name var [])
    where
        newLst = toBasictoken hm lst var

toFct :: [Token] -> HashMap String Var -> String -> [Token]
toFct lst hm var = makeItRedable $ intersperse (Op Add) $ addAll $ filter isNumb $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst var

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

toVar :: Var -> [Token] -> HashMap String Var -> Var
toVar _ [] _ = Void
toVar (Rat name _) lst hm = Rat name (toRat lst hm)
toVar (Ima name _) lst hm = Ima name (toIma lst hm)
toVar (Mat name _) lst hm = Mat name (toMat lst hm)
toVar (Fct name var _) lst hm = Fct name var (toFct lst hm var)
toVar _ _ _ = Void

checkType :: [Token] -> HashMap String Var -> IO Var
checkType lst hm = do
    let bef = takeWhile (/= (Op Equal)) lst
    let aft = dropWhile (/= (Op Equal)) lst
    case aft of
        [] -> do
            putStrLn "Error: Unknown Patern"
            return Void
        _ -> checkType2 bef (tail aft) hm
    where
        checkType2 :: [Token] -> [Token] -> HashMap String Var -> IO Var
        checkType2 ((Var name):[]) lst hm
            | any (\x -> not $ isLetter x) name || name == "i" = do
                putStrLn "Error: Wrong var name"
                return (Void)
            | (Var "i") `elem` lst = checkIma lst hm name
            | (Var "[") `elem` lst = checkMat lst hm name
            | otherwise = checkRat lst hm name
        checkType2 ((Var name):(Op OpenBracket):(Var var):(Op CloseBracket):[]) lst hm = checkFunction var lst hm name
        -- checkType2 tk [(Var "?")] hm = checkCompute (varToNumb tk) hm
        checkType2 _ _ _ = do
            putStrLn "Error: No founded patern"
            return (Void)

getName :: Var -> String
getName (Rat name _) = name
getName (Ima name _) = name
getName (Mat name _) = name
getName (Fct name _ _ ) = name
getName _ = "No name Found"

deployVar :: Token -> HashMap String Var -> Var
deployVar (Var name) hm
    | member name hm = hm ! name
deployVar x _ = Void