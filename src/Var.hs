module Var (toVar, checkType, deployVar, endOfMat, toMat, replaceFctVar, checkBracket, checkMultNumber, Var.isIma, Var.isMat, fctToFloat) where

import Token
import Parsing
import Data
import Data.Char
import Data.List.Split (splitOn)
import Data.List
import Data.HashMap.Strict as Hm (HashMap, member, (!))
import Bracket
import Polish
import CalcExpo
import Debug.Trace

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
    | checkBracket newLst 0 False /= "" = do
        putStrLn $ checkBracket newLst 0 False 
        return Void
    | (checkNumber splitted) = do
        putStrLn "Error: Not same number of element in matrice"
        return Void
    | checkLineCol splitted = do
        putStrLn "Error: Incompatible matrix"
        return Void
    | otherwise = return (Mat name [])
    where
        splitted = map (\x -> read x :: [[Float]]) $  splitOn ("**") $ showTkList $ toMatToken hm $ map toComma newLst
        newLst = toMatToken hm lst
        checkVar :: [Token] -> String
        checkVar [] = []
        checkVar ((Var name):_)
            | name /= ";" && name /= "," && name /= "[" && name /= "]" = name
        checkVar (x:xs) = checkVar xs
        checkOp :: [Token] -> Bool
        checkOp = any (\x -> isOp x && not (isMatricialMult x))
        checkComma :: [Token] -> Bool
        checkComma [] = False
        checkComma (x@(Numb _ _):y@(Var ","):z@(Numb _ _):xs) = checkComma (z : xs)
        checkComma (x@(Var "]"):y@(Var ";"):z@(Var "["):xs) = checkComma xs
        checkComma (x@(Var "]"):y@(Var ","):z@(Var "["):xs) = checkComma xs
        checkComma ((Var "]"):(Var "["):xs) = True
        checkComma ((Numb _ _):(Var "["):xs) = True
        checkComma ((Numb _ _):(Numb _ _):xs) = True
        checkComma ((Var ","):_) = True
        checkComma ((Var ";"):_) = True
        checkComma (x:xs) = checkComma xs
        checkBracket :: [Token] -> Int -> Bool -> String
        checkBracket [] 0 True = ""
        checkBracket [] _ True = "Error: Missmatched Bracket"
        checkBracket [] _ False = "Error: Matrix need a deapth of 2"
        checkBracket lst 2 False = checkBracket lst 2 True
        checkBracket _ n _ 
            | n > 2 = "Error: Matrix need a deapth of 2"
            | n < 0 = "Error: Missmatched Bracket"
        checkBracket ((Var "["):xs) n b = checkBracket xs (n + 1) b
        checkBracket ((Var "]"):xs) n b = checkBracket xs (n - 1) b
        checkBracket ((Op MatricialMult):xs) n b
            | n == 0 = checkBracket xs n b
            | otherwise = "Error: Operator in Matrix"
        checkBracket (_:xs) n b = checkBracket xs n b
        checkNumber :: [[[Float]]] -> Bool
        checkNumber [] = False
        checkNumber (lst:xs)
            | not (all (\y -> length y == size) lst) = True
            | otherwise = checkNumber xs
            where
                size = length $ head lst
        checkLineCol :: [[[Float]]] -> Bool
        checkLineCol [] = False
        checkLineCol (x:[]) = False
        checkLineCol (x:x1:xs)
            | length (head x) == length x1 = checkLineCol (x1:xs)
            | otherwise = True

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
        (Mat _ tk) ->  stringTotokenLst (show tk) ++ toMatToken hm xs
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
toMat x hm = folded
    where
        splitted = map (\x -> read x :: [[Float]]) $ splitOn ("**") $ showTkList $ toMatToken hm $ map toComma x
        folded = foldl1 (\acc x -> matricialMult acc (transpose x)) splitted

toComma :: Token -> Token
toComma (Var ";") = Var ","
toComma x = x

matricialMult :: [[Float]] -> [[Float]] -> [[Float]]
matricialMult [] _ = []
matricialMult (x:xs) cols = matricialMult' x cols : matricialMult xs cols

matricialMult' :: [Float] -> [[Float]] -> [Float]
matricialMult' _ [] = []
matricialMult' line (x:xs) = (sum $ zipWith (\x y -> x * y) line x) : matricialMult' line xs

{-- RATIONAL --}

checkRat :: [Token] -> HashMap String Var -> String -> IO Var
checkRat lst hm name
    | any (== UnParsed) newLst = do
        putStrLn "Error: Parse error"
        return Void
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | isOperatorError lst = do
        putStrLn $ "Error: Operator error"
        return Void
    | getExpo newLst /= 0 = do
        putStrLn $ "Error: Expodent in rationals"
        return Void 
    | otherwise = return (Rat name 0) 
    where
        newLst = toBasictoken hm lst ""

isOperatorError :: [Token] -> Bool
isOperatorError [] = False
isOperatorError ((Op OpenBracket):x@(Op Minus):xs) = isOperatorError (x:xs)
isOperatorError ((Op CloseBracket):x:xs) = isOperatorError (x:xs)
isOperatorError (_:x@(Op OpenBracket):xs) = isOperatorError (x:xs)
isOperatorError ((Op _):(Op _ ):xs) = True
isOperatorError (x:xs) = isOperatorError xs

toRat :: [Token] -> HashMap String Var -> Float
toRat lst hm = simpleReduce $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst ""

{-- IMAGINARY --}

checkIma :: [Token] -> HashMap String Var -> String -> IO Var
checkIma lst hm name
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | any (== UnParsed) newLst = do
        putStrLn "Error: Parse error"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | isOperatorError lst = do
        putStrLn $ "Error: Operator error"
        return Void
    | xPowx newLst 0 = do
        putStrLn $ "Error: Unknown in power"
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
toIma lst hm = makeItRedableRev $ map calcI $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst "i"

calcI :: Token -> Token
calcI (Numb x y)
    | y == 0 = Numb x 0
    | y < 0 && odd y = calcI (Numb x ((-y) + 2)) 
    | even y && even (div y 2) = Numb x 0
    | even y = Numb (-x) 0
    | even (div y 2) = Numb x 1
    | otherwise = Numb (-x) 1
calcI x = x

{-- FUNCTION --}

checkFunction :: String -> [Token] -> HashMap String Var -> String -> IO Var
checkFunction var lst hm name
    | var == "i" || var `elem` constName = do
        putStrLn $ "Error: \"" ++ var ++ "\" is a constant"
        return Void
    | any (== UnParsed) newLst = do
        putStrLn "Error: Parse error"
        return Void
    | any isVar newLst = do
        let (Just x) = find isVar newLst
        putStrLn $ "Error: Var \"" ++ show x ++ "\" not expected"
        return Void
    | any isMatricialMult lst = do
        putStrLn $ "Error: Matrix multiplication not expected"
        return Void
    | isOperatorError lst = do
        putStrLn $ "Error: Operator error"
        return Void
    | xPowx newLst 0 = do
        putStrLn $ "Error: Unknown in power"
        return Void
    | otherwise = return (Fct name var [])
    where
        newLst = toBasictoken hm lst var

toFct :: [Token] -> HashMap String Var -> String -> [Token]
toFct lst hm var = makeItRedable $ intersperse (Op Add) $ addAll $ filter isNumb $ solvePolish $ delBracket $ smallReduce $ toBasictoken hm lst var

{-- MANAGER --}

xPowx :: [Token] -> Int -> Bool
xPowx [] _ = False
xPowx ((Numb _ x):_) n
    | x /= 0 && n /= 0 = True
xPowx ((Op OpenBracket):xs) n
    | n == 0 = xPowx xs n
    | otherwise = xPowx xs (n + 1)
xPowx ((Op Pow):(Op OpenBracket):xs) n = xPowx xs (n + 1)
xPowx ((Op Pow):(Numb _ x):xs) _
    | x /= 0 = True
xPowx ((Op CloseBracket):xs) n
    | n == 0 = xPowx xs n
    | otherwise = xPowx xs (n - 1)
xPowx (x:xs) n = xPowx xs n

replaceFctVar :: Float -> [Token] -> [Token]
replaceFctVar _ [] = []
replaceFctVar v (x@(Numb a b):xs)
    | b == 0 = x : replaceFctVar v xs
    | otherwise = multToken (Numb a 0) (powToken (Numb v 0) (Numb (fromIntegral b) 0)) : replaceFctVar v xs
replaceFctVar v (x:xs) = x : replaceFctVar v xs

fctToToken :: Var -> Float -> Token
fctToToken v value = Numb (fctToFloat v value) 0

fctToFloat :: Var -> Float -> Float
fctToFloat (Fct _ _ tkLst) value = simpleReduce $ solvePolish $ delBracket $ smallReduce $ replaceFctVar value tkLst
fctToFloat (Rat _ x) _ = x

varToToken :: HashMap String Var -> String -> Token -> [Token] 
varToToken hm except x@(Var name)
    | name == except = [(Numb 1 1)]
    | member name hm = case hm ! name of
        (Rat _ value) -> [(Numb value 0)]
        (Fct _ _ tk) -> tk
        (Ima _ tk) -> tk
        _ -> [x] 
varToToken _ _ x  = [x] 

toBasictoken :: HashMap String Var -> [Token] -> String -> [Token]
toBasictoken hm tk except = 
    let maped = foldl (\acc x -> acc ++ varToToken hm except x) [] (transformFct tk)
        ret = case maped of
            ((Op Minus):x1@(Numb _ _):xs) -> appMinus x1 : xs
            _ -> maped
    in toBasictoken2 ret
    where
        transformFct :: [Token] -> [Token]
        transformFct [] = []
        transformFct ((Var name):(Op OpenBracket):(Numb value _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                x@(Fct _ _ _) -> fctToToken x value : transformFct xs
                _ -> [UnParsed]
        transformFct ((Var name):(Op OpenBracket):(Op Minus):(Numb value _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                x@(Fct _ _ _) -> fctToToken x (-value) : transformFct xs
                _ -> [UnParsed]
        transformFct ((Var name):(Op OpenBracket):(Var name2):(Op CloseBracket):xs)
            | member name hm && name2 == except = case hm ! name of
                x@(Fct _ _ tk) -> Op OpenBracket : tk ++ [Op CloseBracket] ++ transformFct xs
            | member name hm && member name2 hm = case (hm ! name, hm ! name2) of
                (x@(Fct _ _ _), y@(Rat _ value)) -> fctToToken x value : transformFct xs
                _ -> [UnParsed]
            | otherwise = [UnParsed]
        transformFct ((Var name):(Op OpenBracket):(Op Minus):(Var name2):(Op CloseBracket):xs) 
            | member name hm && member name2 hm = case (hm ! name, hm ! name2) of
                (x@(Fct _ _ _), y@(Rat _ value)) -> fctToToken x (-value) : transformFct xs
                _ -> [UnParsed]
            | otherwise = [UnParsed]
        transformFct ((Var name):xs)
            | name == except = Numb 1 1 : transformFct xs
            | member name hm = case hm ! name of
                x@(Ima _ tk) -> Op OpenBracket : tk ++ [Op CloseBracket] ++ transformFct xs
                (Rat _ x) -> (Numb x 0) : transformFct xs
                _ -> [UnParsed]
        transformFct (x:xs) = x : transformFct xs
        toBasictoken2 :: [Token] -> [Token]
        toBasictoken2 [] = []
        toBasictoken2 (x@(Op _):(Op Minus):y@(Numb _ _):xs) = x : appMinus y : toBasictoken2 xs
        toBasictoken2 (x@(Numb _ _):y@(Numb _ _):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x@(Op CloseBracket):y@(Numb _ _):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x@(Numb _ _):y@(Op OpenBracket):xs) = x : Op Mult : y : toBasictoken2 xs
        toBasictoken2 (x:xs) = x : toBasictoken2 xs

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
    case bef of
        [(Var "x")] -> putStrLn "Warning: Using \"x\" as variable name can create conflit when computing."
        _ -> return ()
    case aft of
        [] -> do
            putStrLn "Error: Unknown Patern"
            return Void
        _ -> checkType2 bef (tail aft) hm
    where
        checkType2 :: [Token] -> [Token] -> HashMap String Var -> IO Var
        checkType2 ((Var name):[]) lst hm
            | isOp (head lst) && head lst /= (Op Minus) && head lst /= (Op OpenBracket) = do
                putStrLn "Error: Non expected operator"
                return (Void)
            | checkBracket lst 0 = do
                putStrLn "Error: Missmatched bracket"
                return (Void)
            | checkMultNumber lst = do
                putStrLn "Error: Operator is missing"
                return (Void)
            | name == "i" || name `elem` constName = do
                putStrLn $ "Error: \"" ++ name ++ "\" is a constant"
                return (Void)
            | any (\x -> not $ isLetter x) name = do
                putStrLn "Error: Wrong var name"
                return (Void)
            | (Var "i") `elem` lst || any (Var.isIma hm) lst = checkIma lst hm name
            | (Var "[") `elem` lst || any (Var.isMat hm) lst = checkMat lst hm name
            | otherwise = checkRat lst hm name
        checkType2 ((Var name):(Op OpenBracket):(Var var):(Op CloseBracket):[]) lst hm = checkFunction var lst hm name
        checkType2 _ _ _ = do
            putStrLn "Error: Unknown patern"
            return (Void)

checkBracket :: [Token] -> Int -> Bool
checkBracket [] 0 = False
checkBracket [] n = True
checkBracket ((Op OpenBracket):xs) n = checkBracket xs (n + 1)
checkBracket ((Op CloseBracket):xs) n = checkBracket xs (n - 1)
checkBracket (_:xs) n = checkBracket xs n 

checkMultNumber :: [Token] -> Bool
checkMultNumber [] = False
checkMultNumber (x:[]) = False
checkMultNumber ((Numb _ _):(Numb _ _):xs) = True
checkMultNumber (x:xs) = checkMultNumber xs

isIma hm (Var x)
    | member x hm = case hm ! x of
        Ima _ _ -> True
        _ -> False
isIma _ _ = False

isMat hm (Var x)
    | member x hm = case hm ! x of
        Mat _ _ -> True
        _ -> False
isMat _ _ = False

deployVar :: Token -> HashMap String Var -> Var
deployVar (Var name) hm
    | member name hm = hm ! name
deployVar x _ = Void