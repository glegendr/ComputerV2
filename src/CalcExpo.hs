module CalcExpo
(calcX
, getExpo) where

import Token
import Data
import Debug.Trace

getExpo :: [Token] -> Int
getExpo [Numb _ x] = x
getExpo tkLst = 
    if (getExpoF min tkLst < 0)
    then getExpoF min tkLst
    else getExpoF max tkLst
    where
        getExpoF :: (Int -> Int -> Int) -> [Token] -> Int
        getExpoF f = foldl (\ acc x -> case x of
            Numb _ y -> f acc y
            _ -> acc) 0

expo0 :: [Token] -> String
expo0 lst@((Numb c 0):[])
    | c == 0 = "  True"
expo0 [] = "  True"
expo0 _ = "  False"

expo1 :: [Token] -> String
expo1 ((Numb b 1):[]) = "  x = 0"
expo1 ((Numb b 1):_:(Numb c 0):[]) = "  x = " ++ show ((-c) / b)
expo1 _ = "Error during expression calculation"

expo2 :: [Token] -> String
expo2 ((Numb a 2):_:(Numb b 1):_:(Numb c 0):[]) =
    let delta = b ^ 2 - 4 * a * c
    in 
        if (delta == 0)
        then ("  The solution is:\n    " ++ show ((-b) / (2 * a)))
        else if (delta > 0)
        then ("  The two solutions are:\n    " ++ show (((-b) - sqrt delta) / (2 * a)) ++ "\n     " ++ show (((-b) + sqrt delta) / (2 * a)))
        else ("  The two complex solution are:\n    (" ++ show (-b) ++ " - i√" ++ show (-delta) ++ ") / (2 * " ++ show a ++ ")\n    (" ++ show (-b) ++ " + i√" ++ show (-delta) ++ ") / (2 * " ++ show a ++ ")")
expo2 ((Numb a 2):[]) = expo2 $ (Numb a 2) : (Op Add) : (Numb 0 1) : (Op Add) : (Numb 0 0) : []
expo2 ((Numb a 2):o:(Numb b 1):[]) = expo2 $ (Numb a 2) : o : (Numb b 1) : o : (Numb 0 0) : []
expo2 ((Numb a 2):o:(Numb c 0):[]) = expo2 $ (Numb a 2) : o : (Numb 0 1) : o : (Numb c 0) : []
expo2 _ = "Error during expression calculation"

calcX :: [Token] -> String
calcX tkLst =
    let expo = getExpo tkLst
    in
        if (expo == 0)
        then expo0 tkLst
        else if (expo == 1)
        then expo1 tkLst
        else expo2 tkLst