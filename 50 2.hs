--1
enumFromTo :: Int -> Int -> [Int]
enumFromTo x y = if (x<=y) then (x:enumFromTo0 (x+1) y) else []

--2)
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' min x max | min < max = min:enumFromThenTo' ((x+min)-1) x max
                          | otherwise = []

--3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) l [] = l
(+++) (x:xs) l = x : (+++) xs l

--4
(!!) :: [a] -> Int -> a
(!!) (h:t) 0 = h
(!!) (h:t) x = (!!) t (x-1)

--5
reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]

--6
take :: Int -> [a] -> [a]
take n [] = []
take 0 (h:t) = []
take n (h:t) = h : (take (n-1) t)

--7
drop :: Int -> [a] -> [a]
drop n [] = []
drop 0 l = l
drop n (h:t) = (drop (n-1) t)

--8
zip :: [a] -> [b] -> [(a,b)]
zip _ _ = []
zip (h:t) (x:xs) = (h,x) : zip t xs

--9
elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (h:t) = if (n == h) then True else elem n t

--10
replicate :: Int -> a -> [a]
replicate 0 n = []
replicate x n = n : replicate (x-1) n

--11
intersperse :: a -> [a] -> [a]
intersperse n [] = []
intersperse n (h:t) = h : n : intersperse n t 

--12
group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = l1 : group l2 where (l1,l2) = span (==h) (h:t)

--13
concat :: [[a]] -> [a]
concat [[]] = []
concat (h:t) = h ++ concat t

--14
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

--15
tails :: [a] -> [[a]]
tails [] = []
tails (x:t) = (x:t) : tails t

--16
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf l [] = False
isPrefixOf [] l = True
isPrefixOf (x:t) (y:ys) = if x == y then isPrefixOf t ys else False

--17
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)

--18
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf (h:t) (x:y) = if h == x then isSubsequenceOf t y else isSubsequenceOf (h:t) y

--19
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices x l = aux3 0 x l

aux3 :: Eq a => Int -> a -> [a] -> [Int]
aux3 a x [] = []
aux3 a x (h:t) = if (a==h) then x:aux3 a (x+1) t else aux3 a (x+1) t

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t) | x == h = 0 : map (+1) (elemIndices' x t)
    				 | otherwise =  map (+1) (elemIndices' x t)

--20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : filter (/= h) (nub' t)

--21
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (h:t) = if (n==h) then t else h : delete n t

--22
(\\):: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] _ = []
(\\) (h:t) (x:y) = (\\) (delete x (h:t)) y

--23
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union [] [] = []
union (h:t) (x:xs) = (if elem x (h:t)== True) then union (h:t) xs else (union (h:t) xs) ++ [x]
					
--24
intersect :: Eq a => [a] -> [a] -> [a]
intersect l [] = l
intersect [] _ = []
intersect (h:t) (x:xs) = (if elem h (x:xs)==True) then h : intersect t (x:xs) else intersect t (x:xs)

--25
insert :: Ord a => a -> [a] -> [a] 
insert n [] = [n]
insert n (h:t) = if n <= h then (n:h:t) else h: insert n t

--26
unwords :: [String] -> String
unwords [] = []
unwords [x] = x
unwords (h:t) = h ++ " " ++ unwords t
 
--27
unlines :: [String] -> String
unlines  [] = []
unlines [x] = x ++ '\n'
unlines (h:t) = h ++ '\n' ++ unlines t

--28
pMaior :: Ord a => [a] -> Int
pMaior [] = 0 
pMaior (h:t)
    | h > (t !! x) = 0
    | otherwise = 1 + x
    where x = pMaior t 

pMaior0 :: Ord a => [a] -> Int
pMaior0 [x] = 0
pMaior0 (h:t) = pMaior_aux 0 (maximum (h:t)) (h:t)

pMaior_aux :: Eq a => Int -> a -> [a] -> Int
pMaior_aux p x [a] = 0
pMaior_aux p x (h:t) | x==h = p
					 | otherwise = pMaior_aux (p+1) x t 

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
teamRepetidos0 [x] = False
temRepetidos (h:t) = (if elem h t == True) then True else temRepetidos t 

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) = if isdigit x then x : algarismos xs else algarismos xs

isdigit :: Char -> Bool
isdigit x = ord x >= ord '0' && ord x <= ord '9'

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares [x,y]=[y]
posImpares (h:y:t)= y : posImpares t

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares [x,y] = [x]
posPares (h:y:t) = h:pospares t

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (h1:h2:t) = if h1 <= h2 then isSorted (h2:t) else False

--34
iSort :: Ord a => [a] -> [a]
isort [] =[]
iSort l = minimum l : iSort (filter(/=minimum l) l)

--35
menor :: String -> String -> Bool
menor l [] = False
menor [] l = True
menor (h:t) (x:xs)  | h < x  = True
				  	| h > x = False
				  	| otherwise = menor t xs

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((x,y):t) = if n == x then True else elemMSet n t 

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t 

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = replicate y x ++ converteMSet t

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = []
insereMSet x ((a,b):t) = if (x==a) then ((a,b+1):t) else (a,b) : insereMSet x (t)

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMset x [] = []
removeMset x ((a,b):t) 	| ((x==a) && (b>1)) = ((a,b-1):t) 
						| ((x==a) && (b==1)) = (t) 
						| otherwise = (a,b) : removeMset x (t)

--41
constroiMset0 :: Ord a => [a] -> [(a,Int)]
constroiMset0 [] = []
constroiMset0 (h:t) = (h,p1) : constroiMset0 (p2) where
	p1= (length (filter(==h) (h:t)))-- ve quantas vezes o h aparece na lista
	p2= filter (/=h) (h:t) -- fornece a lista sem o h

--42
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls
--43
catMaybes0 :: [Maybe a] -> [a]
catMaybes0 [] = []
catMaybes0 ((Just x):xs) = x : catMaybes xs
catMaybes0 ((Notinhg):xs)= catMaybes xs

--44
data Movimento = Norte | Sul | Este | Oeste 
				deriving Show
posicao0 :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao0 (x,y) [] = (x,y)
posicao0 (x,y) (Norte:t) = posicao0 (x,y+1) t
posicao0 (x,y) (Sul:t) = posicao0 (x,y-1) t
posicao0 (x,y) (Este:t) = posicao0 (x+1,y) t
posicao0 (x,y) (Oeste:t) = posicao0 (x-1,y) t

--45
data Movimento = Norte | Sul | Este | Oeste 
				deriving Show
caminho0 :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho0 (x,y) (w,z) 	| x < w = Este : caminho0 (x+1,y) (w,z)
					  	| x > w = Oeste : caminho0 (x-1,y) (w,z)
					  	| y > z = Sul : caminho0 (x,y-1) (w,z)
					  	| y < z = Norte : caminho0 (x,y+1) (w,z)
					  	| otherwise = []

--46
data Movimento = Norte | Sul | Este | Oeste 
				deriving Show
vertical0 :: [Movimento] -> Bool
vertical0 [] = True 
vertical0 (Norte:t) = vertical0 t
vertical0 (Sul:t) = vertical0 t
vertical0 (Este:t) = False
vertical0 (Oeste:t) = False 

--47 
data Posicao = Pos Int Int
			  deriving Show
maisCentral0 :: [Posicao] -> Posicao
maisCentral0 [(Pos x y)] = Pos x y
maisCentral0 ((Pos x y): (Pos a b) : t) = if  (x^2 + y^2) <=  (a^2 + b^2)  then maisCentral0 ((Pos x y : t) else maisCentral0 ((Pos a b): t)

--48
data Posicao = Pos Int Int
				deriving Show
vizinhos0 :: Posicao -> [Posicao] -> [Posicao]
vizinhos0 (Pos x y) [] = []
vizinhos0 (Pos x y) ((Pos w z):xs) = if (x == (w-1)) && (y==z) || (x== (w+1)) && (y==z) || (y== z+1) && (x==w) || (y== z-1) && (x==w) then (Pos w z) : (vizinhos0 (Pos x y) xs) else (vizinhos (Pos x y) t)

--49
data Posicao = Pos Int Int
				deriving Show
mesmaOrdenada0 :: [Posicao] -> Bool
mesmaOrdenada0 [] = False
mesmaOrdenada0 [x] = True
mesmaOrdenada0 ( (Pos x y): (Pos w z):xs)= if (y==z) then mesmaOrdenada0 ((Pos x y):xs) else False

--50
data Semaforo = Verde | Amarelo | Vermelho
				deriving Show
interseccaook :: [Semoforo] -> Bool
interseccaook x = if count x <=1 then True else False where
	count [] = 0
	count ((Verde):xs)) = 1+ count xs
	count ((Amarelo):xs)) = 1+ count xs
	count ((Vermelho):xs) = count xs





