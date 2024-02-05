import Data.Char
--1


--a)
 --[6,12,18]
--b)
 --[6,12,18]
--c)
 --[(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--d)
 --[1,1,4,4,9,9,16,16,25,25]


--2)
--a)
lista = [ 2^x | x <- [1..10]]

--b)
lista2 = [(x,6-x) | x<-[1..5]]

--c)

--3) 
 
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (letras,h:numeros)
                 | isAlpha h = (h:letras,numeros)
                 |otherwise = (letras,numeros)
                 where (letras,numeros) = digitAlpha t

--4)
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (neg+1,zero,pos)
          | h = 0 = (neg,zero+1,pos)
          | h > 0 = (neg,zero,pos+1)
          where (neg,zero,pos) = nzp t

--5)

divMod :: Integral a => a -> a -> (a, a)
divMod x y | x < y = (0,x)
           | x = y = (1,0)
           | otherwise = (d+1,r)
           where (d,r) = divMod (x-y)y





