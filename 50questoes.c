import Data.List
import Data.Char
import Data.Either
import Data.Maybe

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y = if (x<=y) then (x:enumFromTo' (x+1) y) else []

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x == z = [x]
                      | z < x = []
                      | otherwise = x:enumFromThenTo'(x+(y-1)) y z

--3
junta :: [a] -> [a] -> [a]
junta [] [] = []
junta [] l = l 
junta l [] = l 
junta (h:t) l = h : (junta t l)

--4
posicao :: [a] -> Int -> a
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1) 

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' x [] = []
take' 0 (h:t) = []
take' x (h:t) = h : take' (x-1) t

--7
drop' :: Int -> [a] -> [a]
drop' x [] = []
drop' 0 (h:t) = (h:t)
drop' x (h:t) = drop' (x-1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] l = []
zip' l [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

--9
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' x y = y : replicate' (x-1) y

--10
intersperse' :: a -> [a] -> [a]
intersperse' x [] = []
intersperse' x (h:t) = h:x:intersperse' x t

--11
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = l1 : group' l2 where (l1,l2) = span (==h) (h:t)

--12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits' :: [a] -> [[a]]
inits' [] = []
inits' l = inits' (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:t) = (x:t) : tails' t

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t --necessario esta condiçao pois head [] dá erro
heads' (h:t) = head h : heads' t
            
--16
conta :: [a] -> Int
conta [] = 0
conta (h:t) = 1 + conta t

total' :: [[a]] -> Int
total' [] = 0
total' (h:t) = conta h + total' t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((h1,h2,h3):t) = (h1,h3) : fun t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((h1,h2,h3):t) = h1 ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade x y [] = []
idade x y ((h1,h2):t) = if (x-h2 >= y) then h1 : idade x y t else idade x y t

--20
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n 1 = [1]
powerEnumFrom' n m | m > 1 = powerEnumFrom' n (m-1) ++ [n^(m-1)]
                   | otherwise = []
--21
isPrime' :: Int -> Bool
isPrime' k = if k > 1 then null [x | x <- [2..k-1],k `mod` x == 0] else False

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l = True
isPrefixOf' l [] = False
isPrefixOf' (x:xs) (y:ys) = if x == y then isPrefixOf' xs ys else False

--23
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' l1 l2 = isPrefixOf' (reverse' l1) (reverse' l2)

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' l [] = False
isSubsequenceOf' [] l = True
isSubsequenceOf' (h:t) (x:y) = if h == x then isSubsequenceOf' t y else isSubsequenceOf' (h:t) y

--25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x (h:t) | x == h = 0 : map (+1) (elemIndices' x t) 
                     | otherwise =  map (+1) (elemIndices' x t)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : filter (/= h) (nub' t)

--27
delete' :: Eq a => a -> [a] -> [a]
delete' x [] = []
delete' x (h:t) = if (x==h) then t else h:delete' x t

--28
remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] l = []
remove (h:t) (x:y) = remove (delete x (h:t)) y

--29
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' l [] = l
union' (h:t) (x:xs) = if elem x (h:t) == True then union' (h:t) xs else union' (h:t) xs ++ [x]

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l [] = l
intersect' [] l = []
intersect' (h:t) (x:xs) = if elem h (x:xs) == True then h : intersect' t (x:xs) else intersect' t (x:xs)

--31
insert' :: Ord a => a -> [a] -> [a] 
insert' n [] = [n]
insert' n (h:t) = if n <= h then (n:h:t) else h : insert' n t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ " " ++ unwords' t

--33
unlines' :: [String] -> String
unlines'  [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

--34
pMaior' :: Ord a => [a] -> Int
pMaior' [x] = 0
pMaior' (h:t) = pMaior_aux 0 (maximum (h:t)) (h:t)

pMaior_aux :: Eq a => Int -> a -> [a] -> Int
pMaior_aux p x [a] = 0
pMaior_aux p x (h:t) | x == h = p
                     | otherwise = pMaior_aux (p+1) x t 

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b 
lookup' x [] = Nothing
lookup' x ((h1,h2):t) = if (x == h1) then Just h2 else lookup' x t

--36 
preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' [x] = [x]
preCrescente' (h1:h2:t) |  h1 < h2 = h1 : preCrescente' (h2:t)
                        | otherwise = [h1]

--37
iSort' :: Ord a => [a] -> [a]
isort' [] =[]
iSort' l = minimum l : iSort' (filter(/=minimum l) l)

--38
menor' :: String -> String -> Bool
menor' l [] = False
menor' [] l = True
menor' (h:t) (x:xs) | h < x  = True
                    | h > x = False
                    | otherwise = menor' t xs

--39
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' n [] = False
elemMSet' n ((x,y):t) = if n == x then True else elemMSet' n t

--40
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,y):t) = replicate' y x ++ converteMSet' t

--41
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' x [] = []
insereMSet' x ((a,b):t) = if (x==a) then ((a,b+1):t) else (a,b) : insereMSet' x (t)

--42
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' x [] = []
removeMSet' x ((a,b):t) | ((x==a) && (b>1)) = ((a,b-1):t) 
                       | ((x==a) && (b==1)) = (t) 
                       | otherwise = (a,b) : removeMSet' x (t)

--43
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (h:t) = (h,p1) : constroiMSet' (p2) where
    p1= (length (filter(==h) (h:t)))-- ve quantas vezes o h aparece na lista
    p2= filter (/=h) (h:t) -- fornece a lista sem o h

--44
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls

--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):xs) = x : catMaybes' xs
catMaybes' ((Nothing):xs)= catMaybes' xs

--46
data Movimento = Norte | Sul | Este | Oeste 
                deriving Show
posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y) (Norte:t) = posicao' (x,y+1) t
posicao' (x,y) (Sul:t) = posicao' (x,y-1) t
posicao' (x,y) (Este:t) = posicao' (x+1,y) t
posicao' (x,y) (Oeste:t) = posicao' (x-1,y) t

--47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops pi ms = pi == posicao' pi ms || hasLoops pi (init ms) 

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) | equadrado h = 1 + contaQuadrados t
                     |  otherwise = contaQuadrados t 

equadrado :: Rectangulo -> Bool
equadrado (Rect (x1,y1) (x2,y2)) = abs(y2-y1) == abs(x2-x1)

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2) : t)) = abs (x2-x1) * abs(y2-y1) + areaTotal t

--50
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t 
naoReparar (Razoavel:t) = 1 + naoReparar t 
naoReparar (Avariado:t) = naoReparar t