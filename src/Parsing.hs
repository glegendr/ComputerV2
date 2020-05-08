module Parsing (stringTotokenLst, simpleReduce) where

import Token
import Polish
import Data.Char
import Data

spaceIt :: String -> String
spaceIt [] = []
spaceIt (x:[]) = [x]
spaceIt (x:x1:xs)
    | isNumber x && not (isNumber x1) && x1 /= '.' = x : ' ' : spaceIt (x1:xs)
    | x == '*' && x1 == '*' = x : x1 : ' ' : spaceIt xs
    | not (isNumber x) && x /= '.' && (isNumber x1 || [x1] `elem` map fst allOperatorString) = x : ' ' : spaceIt (x1:xs)
    | [x] `elem` map fst allOperatorString = x : ' ' : spaceIt (x1:xs)
    | (not $ isAlphaNum x) && x /= '.' = x : ' ' : spaceIt (x1:xs)
    | isLetter x && (not $ isLetter x1) = x : ' ' : spaceIt (x1:xs)
    | otherwise = x : spaceIt (x1:xs) 

stringTotokenLst :: String -> [Token]
stringTotokenLst x = map toToken $ words $ spaceIt x

simpleReduce :: [Token] -> Float
simpleReduce tkLst = isSimple $ filter (\x -> case x of
            Numb _ _ -> True
            _ -> False) $ toNormal $ reduce $ toPolish tkLst
    where
        isSimple :: [Token] -> Float
        isSimple (Numb x 0:[]) = x
        isSimple x = 0