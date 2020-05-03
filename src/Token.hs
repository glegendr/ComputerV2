module Token
(getOp
, appMinus
, appMult
, getPrecedence
, makeOp
, toToken
, floatToToken
, intToToken
, isOp
, isVar
, isNumb
, showTkList0
, allOperatorString
, addToken
, minusToken
, multToken
, divToken
, powToken
, modToken
) where

import Data.List.Split
import Data.Char
import Data
import Data.Fixed

allOperatorString = [(show Add, Add), (show Minus, Minus), (show Mult, Mult), (show Div, Div), (show Pow, Pow), (show Mod, Mod), (show Equal, Equal), (show OpenBracket, OpenBracket), (show CloseBracket, CloseBracket)]

showTkList0 :: [Token] -> String
showTkList0 x = showTkList x ++ " = 0.0"

getPrecedence :: Token -> Int
getPrecedence x
    | x == Op Add = 2
    | x == Op Minus = 2
    | x == Op Mult = 3
    | x == Op Div = 3
    | x == Op Pow = 4
    | x == Op Mod = 4
    | otherwise = 0

powToken :: Token -> Token -> Token
powToken hx@(Numb x x1) (Numb y y1)
    | y1 /= 0 = hx
    | x1 /= 0 = Numb x (x1 * round y)
    | otherwise = Numb (x ^ (round y)) 0

addToken :: Token -> Token -> Token
addToken (Numb x x1) (Numb y y1)
    | x + y == 0 && x1 == y1 = Numb 0 0
    | x1 /= y1 = Numb x x1
    | otherwise = Numb (x + y) x1
addToken _ _ = UnParsed

minusToken :: Token -> Token -> Token
minusToken (Numb x x1) (Numb y y1)
    | x - y == 0 = Numb 0 0
    | otherwise = Numb (x - y) x1

divToken :: Token -> Token -> Token
divToken (Numb x x1) (Numb y y1)
    | y == 0 = error "division by 0"
    | otherwise = Numb (x / y) (x1 - y1)

multToken :: Token -> Token -> Token
multToken (Numb x x1) (Numb y y1) = Numb (x * y) (x1 + y1)

modToken :: Token -> Token -> Token
modToken (Numb x x1) (Numb y y1) = Numb (mod' x y) x1

getOp :: Token -> (Token -> Token -> Token)
getOp (Op Add) = addToken
getOp (Op Minus) = minusToken
getOp (Op Mult) = multToken
getOp (Op Div) = divToken
getOp (Op Pow) = powToken
getOp (Op Mod) = modToken
getOp _ = error "OUI OUI FROMAGE 2"

makeOp :: (Token, Token, Token) -> Token
makeOp (hx@(Numb x x1), hy@(Numb y y1), ho@(Op op)) = (getOp ho) hx hy  
makeOp _ = error "OUI OUI FROMAGE"

isOp :: Token -> Bool
isOp (Op _) = True
isOp _ = False

isVar :: Token -> Bool
isVar (Var _) = True
isVar _ = False

isNumb :: Token -> Bool
isNumb (Numb _ _) = True
isNumb _ = False

appMinus :: Token -> Token
appMinus (Op Minus) = Op Add
appMinus (Op Add) = Op Minus
appMinus (Numb x y) = (Numb (-x) y)
appMinus x = x

appMult :: Token -> Token -> Token
appMult m x@(Numb _ _) = multToken m x
appMult _ x = x

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