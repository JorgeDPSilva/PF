import System.Random
import Data.List


--1

--a)

bingo :: IO ()
bingo = do x <- bingoaux []
           print x

bingoaux :: [Int] -> IO [Int]
bingoaux l = if (length l == 90) then do {print ("Ja sairam os numeros todos \n");return l}
             else do t <- getChar
                     if (t == '.') then do x <- randomRIO (1::Int,90)
                                           if (temRepetidos x l) then bingoaux l else do {print (x:l);bingoaux (x:l)}
                     else bingoaux l


temRepetidos :: Int -> [Int] -> Bool
temRepetidos x [] = False
temRepetidos x (h:t) |x==h = True
                     |otherwise = temRepetidos x t

--b)

mastermindaux :: IO ()
mastermindaux = do x <- chave_vencedor
                   y <- putStrLn "insira 4 digitos assim (x,y,z,w)"
                   pedir_chave y
                   if ()


poe_na_lista :: (Int,Int,Int,Int) -> [Int]
poe_na_lista (x,y,z,w) = [x,y,z,w]

chave_vencedor :: IO (Int,Int,Int,Int)
chave_vencedor = do x <- randomRIO (1::Int,9)
                    y <- randomRIO (1::Int,9)
                    z <- randomRIO (1::Int,9)
                    w <- randomRIO (1::Int,9)
                    return (x,y,z,w)

pedir_chave :: (Int,Int,Int,Int) -> IO [Int]
pedir_chave l = do x <- getChar
                   y <- getChar
                   w <- getChar
                   z <- getChar
                   return (poe_na_lista ((convert x),(convert y),(convert z),(convert w)))


convert :: Char -> Int
convert x = ((fromEnum 'x')-(fromEnum '0'))

posicoes_erradas :: [Int] -> [Int] -> String
posicoes_erradas [] [] = " "
posicoes_erradas l1 l2  = "as chaves nas posicoes " ++ "(" ++ verifica l1 l2 1 ++ ")"  ++ " estao erradas"

verifica :: [Int] -> [Int] -> Int -> String
verifica [] [] x = ""
verifica [x] [y] num |x==y = ""
                     |otherwise = (show num)
verifica (h:t) (x:xs) y |h==x = verifica t xs (x+1)
                        |otherwise = (show x) ++ "," ++ verifica t xs (x+1)






