import Data.List

--1

--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = if (f h) then True else any' f t

--b)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f l [] = []
zipWith' f [] l = []
zipWith' f (h:t) (x:xs) = f h x: zipWith' f t xs

--c)

takeWhile' :: (a->Bool) -> [a] -> [a] 
takeWhile' f [] = []
takeWhile' f (h:t) = if (f h) then h: takeWhile' f t else takeWhile' f t

--d)

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) = if (f h) then dropWhile' f t else h: dropWhile' f t

--e)

span :: (a-> Bool) -> [a] -> ([a],[a])
span f [] = ([],[])
span f l = (takeWhile' f l, dropWhile' f l)

--f)

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x [] = []
deleteBy' f x (h:t) = if (f x h) then t else h: deleteBy' f x t

--g) 

mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = myinsert h (mysortOn p t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | p x <= p y = x:y:ys
                                        | otherwise = y : myinsert x ys


type Polinomio = [Monomio]
type Monomio = (Float,Int)
type Grau = Int

--2

--a)

selgrau :: Int -> Polinomio -> Polinomio
selgrau x [] = []
selgrau x ((h,h1):t) = if (x==h1) then (h,h1):selgrau x t else selgrau x t

--b) 
 
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

--c)

grau :: Polinomio -> Int 
grau [] = 0
grau ((h,h1):t) = snd (grau_aux ((h,h1):t))

grau_aux :: Polinomio -> Monomio
grau_aux [(h,h1)] = (h,h1)
grau_aux ((h,h1):(h2,h3):t) = if (h1>h3) then grau_aux ((h,h1):t) else grau_aux ((h2,h3):t)


--d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((h,h1):t) = derivar_monomio (h,h1): deriv t
            where derivar_monomio (x,y) = (x*(fromIntegral y),y-1)

--e)

calcula :: Float -> Polinomio -> Float
calcula x [(w,z)] = w * (x^z)
calcula x ((h,h1):t) = h * (x^h1) + calcula x t


--f)

simp :: Polinomio -> Polinomio
simp [] = []
simp pol = filter (\(x,y)->y/=0) pol

--g)

mult :: Monomio -> Polinomio -> Polinomio
mult mon [] = []
mult (x,y) ((h,h1):t) = multMon (x,y) (h,h1):mult (x,y) t
           where multMon (x,y) (w,z) = (x*w,y+z)


--h)

ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena ((h,h1):t) = (insere (h,h1) (ordena t))

insere :: Monomio -> Polinomio -> Polinomio
insere x [] = [x]
insere (x,y) ((h,h1):t) = if (y>h1) then (x,y):(h,h1):t else (h,h1):insere (x,y) t

--i)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x,x1)] = [(x,x1)]
normaliza ((h,h1):t) = normaliza_aux (h,h1) (normaliza t)

normaliza_aux :: Monomio -> Polinomio -> Polinomio
normaliza_aux (c,e) [] = [(c,e)]
normaliza_aux (c,e) ((h,h1):t) |e==h1 = (c+h,e):t
                               |otherwise = (h,h1):normaliza_aux (c,e) t


--j)

soma :: Polinomio -> Polinomio -> Polinomio
soma [] [] = []
soma l [] = l
soma [] l = l
soma ((h,h1):t) ((c,e):k) = soma t (soma_aux (h,h1) ((c,e):k))


soma_aux :: Monomio -> Polinomio -> Polinomio
soma_aux (c,e) [] = []
soma_aux (c,e) ((h,h1):t) |e==h1 = (c+h,e):t
                          |otherwise = (h,h1):soma_aux (c,e) t


--k)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] [] = []
produto l [] = []
produto [] l = []
produto ((h,h1):t) pol = soma (produto_aux (h,h1) pol) (produto t pol) --acho q nao ta certo.


produto_aux :: Monomio -> Polinomio -> Polinomio
produto_aux (x,y) [] = []
produto_aux (x,y) ((h,h1):t) = (x*h,y+h1): produto_aux (x,y) t


--l)

equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv [] l = False
equiv l [] = False
equiv poli1 poli2  |h==h1 = equiv t xs
                   |otherwise = False
                   where 
                    (h:t) = ordena poli1
                    (h1:xs) = ordena poli2


--3

type Mat a = [[a]]

--a)

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (h:t) = aux (length h) t

aux :: Int -> Mat a -> Bool
aux x  [] = True
aux x (h:t) |x==length h = aux x t
            |otherwise = False


--b)

dimMat :: Mat a -> (Int,Int) 
dimMat [] = (0,0)
dimMat (h:t) = (length h,length (h:t))

--c)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat l [] = l
addMat [] l = l
addMat (h:t) (x:xs) = zipWith (+) h x:addMat t xs

--d)

--as linhas passam a colunas [[1,2,3], [0,4,5], [0,0,6]] = [[1,0,0],[2,2,0],[3,5,6]]
transpose' :: Eq a => Mat a -> Mat a
transpose' [] = []
transpose' m = map head m: transpose (map tail m)

--triSup :: Num a => Mat a -> Bool
--triSup [[]] = False

--triSup_aux :: Mat a -> Bool
--triSup_aux [[]] = False
--triSup_aux ()

--e)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f [] [] = []
zipWMat f m1 m2 = (zipWith f (head m1) (head m2)):(zipWMat f (eliminalist m1) (eliminalist m2))


eliminalist :: Mat a -> Mat a
eliminalist [] = []
eliminalist (h:t) = t














