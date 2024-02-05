
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show


--1)

altura :: BTree a -> Int
altura (Node x Empty Empty) = 1
altura (Node x ae ad) = 1 + max (altura ae) (altura ad)

--b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x ae ad) = 1 + contaNodos ae + contaNodos ad

--c) para ser folha tem q ter Empty Empty

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x ae ad) = folhas ae + folhas ad


--d)

prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 a1 = Empty
prune x (Node y ae ad) = (Node y (prune (x-1) ae) (prune (x-1) ad))

--e)

path :: [Bool] -> BTree a -> [a]
path [] (Node y ae ad) = [y]
path l Empty = []
path (h:t) (Node x ae ad) |h = x : path t ad
                          |otherwise = x: path t ae


--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x ae ad) = (Node x (mirror ad) (mirror ae) )


--g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f a1 Empty = Empty
zipWithBT f Empty a2 = Empty
zipWithBT f (Node x ae ad) (Node y ae1 ad1) = (Node (f x y) (zipWithBT f ae ae1) (zipWithBT f ad ad1))


--h)

{--unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) ae ad) = ((Node a (unzipBT ae) (unzipBT ad)),(Node b (unzipBT ae) (unzipBT ad)),(Node c (unzipBT ae) (unzipBT ad)))--}

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) ae ad) = let (a1,b1,c1) = unzipBT ae
                                   (a2,b2,c2) = unzipBT ad
                                   in ((Node x a1 a2),(Node y b1 b2), (Node z c1 c2))

--2

--a)

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _ ) = x
minimo (Node x ae ad) = minimo ae

--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node x Empty ad) = ad
semMinimo (Node x ae ad) = (Node x (semMinimo ae) ad)

--c)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty ad) = (x,ad)
minSmin (Node x ae ad) = let (m,ae1) = (minSmin ae)
                         in (m,(Node x ae1 ad))

--d)

{--remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y ae ad) | x<y = (Node y (remove x ae) ad)
                        | x>y = (Node y ae (remove x ad))
                        | otherwise = (aux ae ad)
                        where aux :: BTree a -> BTree a
                              aux ae Empty =
                              aux ae ad = let (m,ad') = minSmin ad
                                    in (Node m ae ad')--}


--3)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)
 
a2 = (Node (1500,"Joao",TE,Aprov 17) (Node (1400,"Carlos",ORD,Faltou) Empty Empty) (Node (1550,"Andre",TE,Aprov 11) Empty Empty))

a3 = (Node (1500,"Joao",TE,Rep)(Node (1000,"Antonio",TE,Rep) Empty Empty) Empty)

--a)

inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (n,_,_,_) ae ad) |x==n = True
                                 |x<n = inscNum x ae
                                 |x>n = inscNum x ad

--b)

inscNome :: Nome -> Turma -> Bool
inscNome x Empty = False
inscNome x (Node (_,nome,_,_) ae ad) |x==nome=True
                                     |otherwise = inscNome x ae || inscNome x ad


--c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,no,ORD,_) Empty Empty) = []
trabEst (Node (n,no,MEL,_) Empty Empty) = []
trabEst (Node (n,no,TE,_) ae ad) = [(n,no)] ++ trabEst ae ++ trabEst ad

--d)

nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x (Node (num,_,_,w) ae ad) |x==num = (Just w)
                                |x<num = nota x ae
                                |x>num = nota x ad


--e)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas a1 = ((numalunosFAlt a1)/(numalunos a1)) * 100
 
numalunos :: Turma -> Float
numalunos Empty = 0
numalunos (Node r ae ad) = 1 + numalunos ae + numalunos ad

numalunosFAlt :: Turma -> Float
numalunosFAlt Empty = 0
numalunosFAlt (Node (_,_,_,Aprov x) ae ad) = numalunosFAlt ae + numalunosFAlt ad
numalunosFAlt (Node (_,_,_,Rep) ae ad) = numalunosFAlt ae + numalunosFAlt ad
numalunosFAlt (Node (_,_,_,Faltou) ae ad) = 1 + numalunosFAlt ae + numalunosFAlt ad

--f)

mediaAprov :: Turma -> Float
mediaAprov a = (fromIntegral(somaNota a)/(numalunosAprov a))

numalunosAprov :: Turma -> Float
numalunosAprov Empty = 0
numalunosAprov (Node (_,_,_,Rep) ae ad) = numalunosAprov ae + numalunosAprov ad
numalunosAprov (Node (_,_,_,Faltou) ae ad) = numalunosAprov ae + numalunosAprov ad
numalunosAprov (Node (_,_,_,Aprov x) ae ad) = 1 + numalunosAprov ae + numalunosAprov ad

somaNota :: Turma -> Int
somaNota Empty = 0
somaNota (Node (_,_,_,Aprov x) ae ad) = x + somaNota ae + somaNota ad

--g) Numero de alunos aprovados por numero de alunos avaliados

numAlunosAval :: Turma -> Int
numAlunosAval Empty = 0
numAlunosAval (Node (_,_,_,Aprov x) ae ad) = 1 + numAlunosAval ae + numAlunosAval ad
numAlunosAval (Node (_,_,_,Rep) ae ad) = 1 + numAlunosAval ae + numAlunosAval ad
numAlunosAval (Node (_,_,_,Faltou) ae ad) = numAlunosAval ae + numAlunosAval ad

    
racio :: Turma -> Float
racio turma = (numalunosAprov turma)/(fromIntegral (numAlunosAval turma))

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = racio turma


