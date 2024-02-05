data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
            deriving Show
--1

e = Mult (Mais (Const 5) (Const 3)) (Menos (Const 2) (Simetrico (Const 2)))

--a)
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = - (calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--b)

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(" ++ "-" ++ (infixa x) ++ ")"
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) ="(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

--c)

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = (posfixa x) ++ "+"
posfixa (Mais x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "+"
posfixa (Menos x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "-"
posfixa (Mult x y) = (posfixa x) ++ " " ++ (posfixa y) ++ "*"




--2

data RTree a = R a [RTree a]

--a)
soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)

--b)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l)  = 1 + maximum (map altura l)

--c)

prune :: Int -> RTree a -> RTree a
prune 0 (R x _) = R x []
prune _ (R x []) = R x []
prune n (R x l) = R x (map (prune (n-1)) l)

--d)

mirror :: RTree a -> RTree a 
mirror (R x l) = R x (reverse(map mirror l))

--e)

postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = concat(map postorder l) ++ [x]


--3

--a)

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show


ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork ae ad) = (ltSum ae) + (ltSum ad)

--b)

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork ae ad) = (listaLT ae) ++ (listaLT ad)

--c)

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork ae ad) = 1+ (max (ltHeight ae) (ltHeight ad))

--lt1 = (Fork(Fork(Fork(Tip 1)(Fork(Tip 2)(Fork (Tip 3))))(Fork(Fork(Tip 4)(Fork(Tip 5)))))

--4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

a2 = (No 1 (No 2 (No 4 (Leaf "A") (Leaf "C")) (No 5 (Leaf "D") (Leaf "C"))) (No 3 (Leaf "F") (No 6 (Leaf "G") (Leaf "H"))))
--a)

splitFTree :: FTree a b -> (BTree a, LTree b) 
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No y ae ad) = let (bte,lte) = (splitFTree ae)
                              (btd,ltd) = (splitFTree ad)
                          in ((Node y bte btd),(Fork lte ltd))
                          

--b)

{--joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees _ (Tip x) = Just (Leaf x)
joinTrees Empty _ = Nothing
joinTrees (Node x bte btd) (Fork lte ltd) = let re = joinTrees bte lte
                                                rd = joinTrees btd ltd
                                            in case re of Nothing -> Nothing
                                                          (Just ae) -> case rd of 
                                                                        Nothing -> Nothing
                                                                        (Just ad) -> (No x ae ad)--}   










