import Data.List
import Data.Char
import Data.Either
import Data.Maybe

powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' x 1 = [1]
powerEnumFrom' x y | y > 1 = powerEnumFrom' x (y-1) ++ [x^(y-1)]
                   | otherwise = []