module Token
(getOp
, appMinus
, appMult
, appDiv
, getPrecedence
, getPrecedenceOp
, makeOp
, toToken
, floatToToken
, intToToken
, allOperatorString
, addToken
, minusToken
, multToken
, divToken
, powToken
, modToken
, makeItRedable
, makeItRedableRev
, toPositiv
, addAll
) where

import Data.List.Split
import Data.Char
import Data
import Data.List
import Data.Fixed


allOperatorString = foldl (\acc x -> (show x, x):acc) [] [Add, Minus, Mult, Div, Pow, Mod, MatricialMult, Equal, OpenBracket, CloseBracket]

getPrecedence :: Token -> Int
getPrecedence x
    | x == Op Add = 2
    | x == Op Minus = 2
    | x == Op Mult = 3
    | x == Op Div = 3
    | x == Op Pow = 5
    | x == Op Mod = 4
    | otherwise = 0

getPrecedenceOp :: Operator -> Int
getPrecedenceOp x = getPrecedence (Op x)

powToken :: Token -> Token -> Token
powToken hx@(Numb x x1) (Numb y y1)
    | y1 /= 0 = UnParsed
    | y == 1 = hx
    | y < 0 = Numb (1 / (x ^ (- (round y)))) (x1 * round y)
    | y == 0 = (Numb 1 0)
    | otherwise = Numb (x ^ round y) (x1 * round y)

addToken :: Token -> Token -> Token
addToken (Numb x x1) (Numb y y1)
    | x + y == 0 && x1 == y1 = Numb 0 x1
    | x1 /= y1 = Numb x x1
    | otherwise = Numb (x + y) x1
addToken _ _ = UnParsed

minusToken :: Token -> Token -> Token
minusToken (Numb x x1) (Numb y y1)
    | x - y == 0 = Numb 0 0
    | otherwise = Numb (x - y) x1

divToken :: Token -> Token -> Token
divToken (Numb x x1) (Numb y y1) = Numb (x / y) (x1 - y1)

multToken :: Token -> Token -> Token
multToken (Numb x x1) (Numb y y1) = Numb (x * y) (x1 + y1)

modToken :: Token -> Token -> Token
modToken (Numb x x1) (Numb y y1)
    | y == 0 = UnParsed
    | x > 0 = Numb (mod' x y) x1
    | otherwise = Numb (-(mod' (-x) y)) x1

getOp :: Token -> (Token -> Token -> Token)
getOp (Op Add) = addToken
getOp (Op Minus) = minusToken
getOp (Op Mult) = multToken
getOp (Op Div) = divToken
getOp (Op Pow) = powToken
getOp (Op Mod) = modToken
getOp _ = addToken

makeOp :: (Token, Token, Token) -> Token
makeOp (hx@(Numb x x1), hy@(Numb y y1), ho@(Op op)) = (getOp ho) hx hy  
makeOp (x, _, _) = x

appMinus :: Token -> Token
appMinus (Op Minus) = Op Add
appMinus (Op Add) = Op Minus
appMinus (Numb x y) = (Numb (-x) y)
appMinus x = x

appMult :: Token -> Token -> Token
appMult m x@(Numb _ _) = multToken m x
appMult _ x = x

appDiv :: Token -> Token -> Token
appDiv m@(Numb _ _) x@(Numb _ _) = divToken m x
appDiv x _ = x

operatorFromString :: String -> Operator
operatorFromString str = oFS2 str allOperatorString
    where 
        oFS2 str [] = error $ "Unknow value: " ++ str 
        oFS2 str ((name, value):xs)
            | str == name = value
            | otherwise = oFS2 str xs

numbFromString :: String -> Token
numbFromString [] = UnParsed
numbFromString str
        | length splitted > 2 = UnParsed
        | length splitted == 1 = Numb (read str) 0
        | all (== "") splitted = Numb 1 1
        | splitted !! 1 == "" && head (splitted !! 0) == '-' = appMinus $ Numb (read $ tail $ splitted !! 0) 1
        | splitted !! 1 == "" = Numb (read $ splitted !! 0) 1
        | splitted !! 0 == "" = Numb 1 (read $ tail $ splitted !! 1)
        | head (splitted !! 0) == '-' = appMinus $ Numb (read $ tail $ splitted !! 0) (read $ tail $ splitted !! 1)
        | otherwise = Numb (read $ splitted !! 0) (read $ tail $ splitted !! 1)
        where splitted = splitOn "x" str

floatToToken :: Float -> Token
floatToToken x = Numb x 0

intToToken :: Int -> Token
intToToken x = Numb (fromIntegral x) 0

toToken :: String -> Token
toToken str
    | isOperator str = Op $ operatorFromString str
    | isNumb str = numbFromString $ map toLower str
    | otherwise = Var $ map toLower str
    where 
        isNumb [] = False
        isNumb str
            | any (\x -> isNumber x == False && x /= '.') str = False
            | length (filter (== '.') str) > 1 = False
            | otherwise = not $ head str == '.' || last str == '.'
            where
        isOperator [] = False
        isOperator str
            | str `elem` map (\(x, y) -> x) allOperatorString = True
            | otherwise = False

delSameExpo :: Token -> [Token] -> [Token]
delSameExpo _ [] = []
delSameExpo x@(Numb _ a) (y@(Numb _ b):xs)
    | a == b = delSameExpo x xs
    | otherwise = y : delSameExpo x xs
delSameExpo _ _ = [UnParsed]

toPositiv :: [Token] -> [Token]
toPositiv [] = []
toPositiv ((Op Minus):x:xs) = (Op Add) : appMinus x : toPositiv xs
toPositiv (x:xs) = x : toPositiv xs

addAll :: [Token] -> [Token]
addAll [] = []
addAll (x:xs) = foldl addToken x xs : addAll (delSameExpo x xs)

makeItRedable :: [Token] -> [Token]
makeItRedable = changeOp . intersperse (Op Add) . reverse . sortOn (\(Numb _ x) -> x) . addAll . filter isNotOp . toPositiv

makeItRedableRev :: [Token] -> [Token]
makeItRedableRev = changeOp . intersperse (Op Add) . sortOn (\(Numb _ x) -> x) . addAll . filter isNotOp . toPositiv

changeOp :: [Token] -> [Token]
changeOp [] = []
changeOp (a@(Op _):b@(Numb x _):xs)
    | x < 0 = Op Minus : appMinus b : changeOp xs
    | otherwise = a : b : changeOp xs
changeOp (x:xs) = x : changeOp xs