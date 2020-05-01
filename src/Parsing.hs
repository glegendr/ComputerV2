module Parsing
(getAll
, stringTotokenLst
, simpleReduce) where

import Token
import Polish
import Data.Char
import Debug.Trace
import Data

data ParseError = MultEq | NoEq | MultNumb | MultOp | LastOp | FirstOp | UnknowError | None deriving (Eq, Show)

matchError :: ([Token], ParseError) -> [Token]
matchError (ret, err)
    | err == None = ret
    | err == NoEq = ret ++ [Op Equal, Numb 0 0]
    | otherwise = error $ show err

spaceIt :: String -> String
spaceIt [] = []
spaceIt (x:[]) = [x]
spaceIt (x:x1:xs)
    | isNumber x && not (isNumber x1) && x1 /= '.' = x : ' ' : spaceIt (x1:xs)
    | not (isNumber x) && x /= '.' && (isNumber x1 || [x1] `elem` map fst allOperatorString) = x : ' ' : spaceIt (x1:xs)
    | [x] `elem` map fst allOperatorString = x : ' ' : spaceIt (x1:xs)
    | (not $ isAlphaNum x) && x /= '.' = x : ' ' : spaceIt (x1:xs)
    | isLetter x && (not $ isLetter x1) = x : ' ' : spaceIt (x1:xs)
    | otherwise = x : spaceIt (x1:xs) 

stringTotokenLst :: String -> [Token]
stringTotokenLst x = map toToken $ words $ spaceIt x

getAll :: String -> [Token]
getAll str = 
    let all = map toToken $ words $ spaceIt str
    in toNormal $ reduce $ toPolish $ matchError $ isWellFormed all

simpleReduce :: [Token] -> Float
simpleReduce tkLst = isSimple $ filter (\x -> case x of
            Numb _ _ -> True
            _ -> False) $ toNormal $ reduce $ toPolish tkLst
    where
        isSimple :: [Token] -> Float
        isSimple (Numb x 0:[]) = x
        isSimple x = trace (show x) $ 0

isWellFormed :: [Token] -> ([Token], ParseError)
isWellFormed [] = ([], None)
isWellFormed tokenList
    | isOp (head tokenList) = (tokenList, FirstOp)
    | length (filter (== (Op Equal)) tokenList) > 1 = (tokenList, MultEq)
    | (not $ (Op Equal) `elem` tokenList) && ret == None = (tokenList, NoEq)
    | otherwise = (tokenList, ret)
    where
        ret = iSF2 tokenList
        iSF2 :: [Token] -> ParseError
        iSF2 ((Op x):[]) = LastOp
        iSF2 (UnParsed:[]) = UnknowError
        iSF2 (_:[]) = None
        iSF2 (_:(Op _):[]) = LastOp
        iSF2 ((Numb _ _ ):(Op _):xs) = iSF2 xs
        iSF2 ((Op _):(Op _):_) = MultOp
        iSF2 ((UnParsed):_) = UnknowError
        iSF2 _ = MultNumb