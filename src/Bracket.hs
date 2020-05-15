module Bracket
(genListNM
, listXPow
, delBracket
, smallReduce
, multinomialResolution)where

import Token
import Data
import Data.List
import Polish
import Debug.Trace

addList :: Int -> [Int] -> [[Int]]
addList _ [] = []
addList n lst
    | lst == (take takeIt [0,0..] ++ [n]) = lst : []
    | otherwise = lst : addList n (addList2 n lst False)
    where
        takeIt = (length lst - 1) 
        addList2 :: Int -> [Int] -> Bool -> [Int]   
        addList2 _ [] _ = []
        addList2 n (x:x1:xs) b
            | x == n && b == True = x : x1 : xs
            | x >= n      = 0 : addList2 n ((x1 + 1) : xs) True
            | b == False  = (x + 1) : addList2 n (x1:xs) True
        addList2 n (x:xs) True = x : addList2 n xs True
        addList2 n (x:xs) False = (x + 1) : addList2 n xs True

genListNM :: Int -> Int -> [[Int]]
genListNM n m = [x | x <- addList n $ take m [0,0..], sum x == n]

factorial :: Float -> Float
factorial 0 = 1
factorial n = n * factorial (n - 1)

listXPow :: [Token] -> Int -> [Token]
listXPow lst pow = [x | x@(Numb a b) <- listXPow2 lst pow (genListNM pow (length lst)), a /= 0]
    where
        listXPow2 :: [Token] -> Int -> [[Int]] -> [Token] 
        listXPow2 _ _ [] = []
        listXPow2 lst pow (x:xs) =
            let factorialRet = floatToToken $ (factorial (realToFrac pow)) / (product $ map factorial $ map realToFrac x)
                powRet = foldl1 multToken $ map (\(x, y) -> powToken x (intToToken y)) (zip lst x)
                ret = multToken factorialRet powRet
            in ret : listXPow2 lst pow xs

multinomialResolution :: [Token] -> Token -> [Token]
multinomialResolution lst (Numb x 0) =
    let ret = listXPow (filter isNumbNotNull (toPositiv lst)) (round x)
    in intersperse (Op Add) ret
multinomialResolution lst _ = [UnParsed]

binomialResolution :: [Token] -> Token -> [Token]
binomialResolution lst (Numb n 0) = intersperse (Op Add) $ binomialSum 0 (realToFrac $ round n) (filter isNumbNotNull (toPositiv lst))
    where
        binomialSum :: Float -> Float -> [Token] -> [Token]
        binomialSum k n lst@(x:y:[])
            | k > n = []
            | otherwise =
                let factorialRet = floatToToken $ (factorial  n) / ((factorial (n - k)) * factorial k)
                    powRet = multToken (powToken x (floatToToken (n - k))) (powToken y (floatToToken k))
                in multToken factorialRet powRet : binomialSum (k + 1) n lst
        binomialSum _ _ _ = []

smallReduce :: [Token] -> [Token]
smallReduce x = smallReduce2 [] x
    where
        smallReduce2 ret [] = ret
        smallReduce2 ret (a@(Op OpenBracket):b:c@(Op CloseBracket):xs) = smallReduce (ret ++ [b] ++ xs)
        smallReduce2 ret (a@(Numb _ _):b@(Op _):c@(Numb _ _):d@(Op CloseBracket):xs)
            | isCompatible (a, c, b) = smallReduce (ret ++ ((getOp b) a c : d : xs))
            | otherwise = smallReduce2 (ret ++ [a]) (b:c:d:xs) 
        smallReduce2 ret (a@(Numb _ _):b@(Op _):c@(Numb _ _):d@(Op _):xs)
            | getPrecedence b >= getPrecedence d && isCompatible (a, c, b) = smallReduce (ret ++ ((getOp b) a c : d : xs))
        smallReduce2 ret (x:xs) = smallReduce2 (ret ++ [x]) xs

multBracket :: [Token] -> [Token]
multBracket [] = []
multBracket (a@(Op CloseBracket):b@(Op OpenBracket):xs) = a : Op Mult : b : multBracket xs
multBracket (x:xs) = x : multBracket xs

delBracket :: [Token] -> [Token]
delBracket [] = []
delBracket rawLst =
    if ((length $ filter (== (Op OpenBracket)) inBr) > 1)
    then delBracket $ befBr ++ ((Op OpenBracket) : (delBracket (tail $ init $ inBr)) ++ [Op CloseBracket]) ++ aftBr
    else if (inBr == [Op CloseBracket])
    then befBr
    else if (getPrecedence op >= getPrecedence op2)
    then
        let (d, ret) = resolveOpBracket (transformInBracket inBr) op value True
        in delBracket ((reverse $ drop d $ reverse befBr) ++ ret ++ aftBr)
    else
        let (d, ret) = resolveOpBracket (transformInBracket inBr) op2 value2 False
        in delBracket (befBr ++ ret ++ (drop d aftBr))
    where
            lst = multBracket rawLst
            befBr = takeWhile (/= Op OpenBracket) lst
            rawAftBr = drop (findCloseBr (dropWhile (/= Op OpenBracket) lst)) $ dropWhile (/= Op OpenBracket) lst
            rawInBr = (take (findCloseBr (dropWhile (/= Op OpenBracket) lst)) $ dropWhile (/= Op OpenBracket) lst) ++ [Op CloseBracket]
            inBr = rawInBr
            aftBr = case rawAftBr of
                [] -> []
                x -> tail x
            op = case befBr of
                [] -> (Op Add)
                x -> last x
            value =
                if (length befBr >= 2)
                then befBr !! (length befBr - 2)
                else (Numb 0 0)
            op2 = case aftBr of
                [] -> (Op Add)
                x -> head x
            value2 =
                if (length aftBr >= 2)
                then aftBr !! 1
                else (Numb 0 0)
            transformInBracket :: [Token] -> [Token]
            transformInBracket [] = []
            transformInBracket lst
                | length newLst < 2 = lst
                | any (\x -> getPrecedence x /= getPrecedence (Op Add)) (init $ tail newLst) = lst
                | otherwise = let tmp = intersperse (Op Add) $ filter isNumbNotNull $ addAll $ filter isNumbNotNull $ toPositiv (init $ tail lst)
                    in Op OpenBracket : tmp ++ [Op CloseBracket]
                where newLst = filter isOp lst
            resolveOpBracket :: [Token] -> Token -> Token -> Bool -> (Int, [Token])
            resolveOpBracket lst (Op Add) _ _ = (0, tail $ init $ lst)
            resolveOpBracket lst (Op Minus) _ _ = (0, appMinusBracket (tail $ init $ lst) 0)
                where
                    appMinusBracket :: [Token] -> Int -> [Token]
                    appMinusBracket [] _ = []
                    appMinusBracket (x@(Op OpenBracket):xs) br = x : appMinusBracket xs (br + 1)
                    appMinusBracket (x@(Op CloseBracket):xs) br = x : appMinusBracket xs (br - 1)
                    appMinusBracket (x@(Op _):xs) 0 = appMinus x : appMinusBracket xs 0
                    appMinusBracket (x:xs) br = x : appMinusBracket xs br
            resolveOpBracket lst (Op Pow) value@(Numb v _) _
                | v <= 0 = (2, [Numb 1 0])
                | v == 1 = (2, lst)
                | length (filter isNumbNotNull  (tail $ init $ lst)) == 2 = (2, (Op OpenBracket) : binomialResolution (tail $ init $ lst) value ++ [Op CloseBracket])
                | otherwise = (2, (Op OpenBracket) : multinomialResolution (tail $ init $ lst) value ++ [Op CloseBracket])
            resolveOpBracket lst (Op Mult) value@(Numb v x) _
                | v == 0 = (2, [Numb 0 0])
                | v == 1 && x == 0 = (2, lst)
                | otherwise = (2, (Op OpenBracket) : map (appMult value) (tail $ init $ lst) ++ [Op CloseBracket])
            resolveOpBracket lst (Op Mult) (Op OpenBracket) _ =
                let secOp = last $ take (findCloseBr (tail aftBr) + 2) (tail $ aftBr)
                    multBy = if (secOp /= Op Pow)
                        then take (findCloseBr (tail aftBr) + 1) (tail $ aftBr)
                        else take (findCloseBr (tail aftBr) + 3) (tail $ aftBr)
                    size = length multBy
                in (size + 1, multBr lst (delBracket multBy))
            resolveOpBracket lst (Op Div) value@(Numb v _) True
                | v == 0 = (2, [Numb 0 0])
                | otherwise = (2, (Op OpenBracket) : intersperse (Op Add) (map (\x -> appDiv value x) (filter isNumb $ toPositiv $ tail $ init $ lst)) ++ [Op CloseBracket])
            resolveOpBracket lst (Op Div) value@(Numb v _) False
                | v == 1 = (2, lst)
                | otherwise = (2, (Op OpenBracket) : map (\x -> appDiv x value) (tail $ init $ lst) ++ [Op CloseBracket])
            resolveOpBracket lst (Op Div) (Op OpenBracket) _ =
                let divBy = take (findCloseBr $ tail aftBr) (tail $ aftBr) ++ [Op CloseBracket]
                    size = length divBy
                in
                    if (lst == divBy)
                    then (size, [Numb 1 0])
                    else (0, [UnParsed])
            resolveOpBracket _ _ _ _ = (0, [UnParsed])

multBr :: [Token] -> [Token] -> [Token]
multBr lst lst2 = Op OpenBracket : (intersperse (Op Add) $ multBr2 (filter isNumb lst) (filter isNumb lst2)) ++ [Op CloseBracket]
    where
        multBr2 :: [Token] -> [Token] -> [Token]
        multBr2 [] _ = []
        multBr2 (x:xs) lst =  map (appMult x) lst ++  multBr2 xs lst

divBr :: [Token] -> [Token] -> [Token]
divBr lst lst2 = Op OpenBracket : (intersperse (Op Add) $ divBr2 (filter isNumb lst) (filter isNumb lst2)) ++ [Op CloseBracket]
    where
        divBr2 :: [Token] -> [Token] -> [Token]
        divBr2 [] _ = []
        divBr2 (x:xs) lst =  map (appDiv x) lst ++ divBr2 xs lst      