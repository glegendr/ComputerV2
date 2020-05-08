module Polish
(toPolish
, toNormal
, reduce
, isCompatible
, allLeft
, solvePolish
, findCloseBr)where

import Token
import Data

findCloseBr :: [Token] -> Int
findCloseBr lst = findCloseBr2 lst 0
    where
        findCloseBr2 :: [Token] -> Int -> Int
        findCloseBr2 [] _ = 0
        findCloseBr2 (x:xs) br
            | br < 0 = 0
        findCloseBr2 ((Op CloseBracket):xs) br
            | br == 1 = 0
            | otherwise = 1 + findCloseBr2 xs (br - 1)
        findCloseBr2 ((Op OpenBracket):xs) br = 1 + findCloseBr2 xs (br + 1)
        findCloseBr2 (x:xs) br = 1 + findCloseBr2 xs br

allLeft :: [Token] -> Bool -> [Token]
allLeft [] _ = []
allLeft ((Op Equal):x@(Op OpenBracket):xs) _ = Op Minus : x : take (findCloseBr $ x:xs) (x:xs) ++ [Op CloseBracket] ++ allLeft (drop ((findCloseBr $ x:xs) + 1) (x:xs)) True
allLeft ((Op Equal):x1:xs) _ = Op Add : appMinus x1 : allLeft xs True
allLeft (a@(Op _ ):x@(Op OpenBracket):xs) True = appMinus a : x : take (findCloseBr $ x:xs) (x:xs) ++ [Op CloseBracket] ++ allLeft (drop (findCloseBr $ x:xs) (x:xs)) True
allLeft ((Op Add):x1:xs) True = Op Add : appMinus x1 : allLeft xs True
allLeft ((Op Minus):x1:xs) True = Op Add : x1 : allLeft xs True
allLeft ((Op x):x1:xs) True = Op x : x1 : allLeft xs True
allLeft ((Op Minus):x1:xs) False = Op Add : appMinus x1 : allLeft xs False
allLeft (x:xs) False = x : allLeft xs False
allLeft (x:xs) _ = error $ show x

toPolish :: [Token] -> [Token]
toPolish tkLst = toPolish2 (allLeft tkLst False) []
    where
        toPolish2 :: [Token] -> [Token] -> [Token]
        toPolish2 [] [] = []
        toPolish2 [] (x:xs) = x : toPolish2 [] xs
        toPolish2 (hx@(Numb _ _):xs) opLst = hx : toPolish2 xs opLst
        toPolish2 (hx@(Var _):xs) opLst = hx : toPolish2 xs opLst
        toPolish2 ((Op o):xs) [] = toPolish2 xs [Op o]
        toPolish2 (hy@(Op OpenBracket):xs) allOp = toPolish2 xs (hy:allOp)
        toPolish2 (hy@(Op CloseBracket):xs) allOp = takeWhile (/= (Op OpenBracket)) allOp ++ toPolish2 xs (tail $ dropWhile (/= (Op OpenBracket)) allOp)
        toPolish2 (hy@(Op o):xs) allOp@(y:ys)
            | getPrecedence hy <= getPrecedence y = y : toPolish2 (hy:xs) ys
            | otherwise = toPolish2 xs (hy:allOp)
        toPolish2 _ _ = [UnParsed]

isCompatible :: (Token, Token, Token) -> Bool
isCompatible ((Numb x x1), (Numb y y1), (Op z))
    | (z == Add || z == Minus || z == Mod) && x1 == y1 = True
    | (z == Mult || z == Div) = True
    | z == Pow && y1 == 0 = True
isCompatible _ = False

reduce :: [Token] -> [Token]
reduce x = reduce2 [] x
    where
        reduce2 :: [Token] -> [Token] -> [Token]
        reduce2 ret [] = ret
        reduce2 ret (x:x1:x2:xs)
            | isCompatible (x, x1, x2) = reduce (ret ++ (makeOp (x, x1, x2): xs))
        reduce2 ret (x:x1:x2:x3:xs)
            | isCompatible (x, x2, x3) && getPrecedence x1 == getPrecedence x3 = reduce (ret ++ (makeOp (x, x2, x3): x1 : xs))
        reduce2 ret (x:x1@(Numb _ _):x2:x3:x4:xs)
            | isCompatible (x, x3, x4) && getPrecedence x2 == getPrecedence x4 = reduce (ret ++ (makeOp (x, x3, x4) : x1 : x2 : xs))
        reduce2 ret (x@(Numb _ _):x1:x2:x3:x4:xs)
            | isCompatible (x1, x3, x4) && getPrecedence x2 == getPrecedence x4 = reduce (ret ++ (x : makeOp (x1, x3, x4) : x2 : xs))
        reduce2 ret (x:xs) = reduce2 (ret ++ [x]) xs

toNormal :: [Token] -> [Token]
toNormal [] = []
toNormal (x@(Numb _ _):x1@(Numb _ _):x2@(Op _):xs) = toNormal $ x:x2:x1:xs
toNormal (x:xs) = x : toNormal xs

solvePolish :: [Token] -> [Token]
solvePolish = toNormal . reduce . toPolish