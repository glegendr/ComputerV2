module CalcExpo
(calcX
, getExpo) where

import Token
import Data

getExpo :: [Token] -> Int
getExpo tkLst = 
    if (getExpoF min tkLst < 0)
    then getExpoF min tkLst
    else getExpoF max tkLst
    where
        getExpoF :: (Int -> Int -> Int) -> [Token] -> Int
        getExpoF f = foldl (\ acc x -> case x of
            Numb _ y -> f acc y
            _ -> acc) 0

expo0 :: [Token] -> IO ()
expo0 lst@((Numb c 0):[])
    | c == 0 = putStrLn "True"
expo0 [] = putStrLn "True"
expo0 _ = putStrLn "False"

expo1 :: [Token] -> IO ()
expo1 ((Numb b 1):[]) = putStrLn "x = 0"
expo1 ((Numb b 1):_:(Numb c 0):[]) = putStrLn $ "x = " ++ show ((-c) / b)
expo1 _ = putStrLn "X Error"

expo2 :: [Token] -> IO ()
expo2 ((Numb a 2):_:(Numb b 1):_:(Numb c 0):[]) = do
    let delta = b ^ 2 - 4 * a * c
    if (delta == 0)
    then putStrLn $ "The solution is:\n" ++ show ((-b) / (2 * a)) 
    else if (delta > 0)
    then (
        do
            putStrLn $ "The two solutions are:"
            print $ ((-b) - sqrt delta) / (2 * a)
            print $ ((-b) + sqrt delta) / (2 * a))
    else (
        do
            putStrLn $ "The two complex solution are:"
            putStrLn $ "(" ++ show (-b) ++ " - i√" ++ show (-delta) ++ ") / ( 2 * " ++ show a ++ ")"
            putStrLn $ "(" ++ show (-b) ++ " + i√" ++ show (-delta) ++ ") / ( 2 * " ++ show a ++ ")")
expo2 ((Numb a 2):[]) = expo2 $ (Numb a 2) : (Op Add) : (Numb 0 1) : (Op Add) : (Numb 0 0) : []
expo2 ((Numb a 2):o:(Numb b 1):[]) = expo2 $ (Numb a 2) : o : (Numb b 1) : o : (Numb 0 0) : []
expo2 ((Numb a 2):o:(Numb c 0):[]) = expo2 $ (Numb a 2) : o : (Numb 0 1) : o : (Numb c 0) : []
expo2 _ = putStrLn "X Error"

calcX :: [Token] -> IO ()
calcX tkLst = do
    let expo = getExpo tkLst
    if (expo == 0)
    then expo0 tkLst
    else if (expo == 1)
    then expo1 tkLst
    else expo2 tkLst