import Data.List
--1

data Frac = F Integer Integer

--a)
mdc :: Integer -> Integer -> Integer
mdc x y |mod x y /= 0 = mdc y (mod x y)
        |otherwise=abs y


normaliza :: Frac -> Frac
normaliza (F x y) = F (x `div` h) (y `div` h)
              where h = mdc (abs x) (abs y) 


--b)

instance Eq Frac where
     (==) (F x y) (F x1 y1) = x+x1==y+y1


--c)
instance Ord Frac where
     (<=) (F x y) (F z w) = (x*w) <= (y*z)

--d)

instance Show Frac where
     show (F x y) = "(" ++ show x ++ "/" ++ show y ++ ")"

--e)

somafrac :: Frac -> Frac -> Frac
somafrac (F x y) (F z w) = normaliza(F ((x*w)+(z*y)) (y*w))

subfrac :: Frac -> Frac -> Frac
subfrac (F x y) (F z w) = normaliza (F ((x*w)-(z*y)) (y*w))

sinalfrac ::  Frac -> Frac
sinalfrac (F x y) =  if (x==0) then 0 else (if ( y<0 || x<0) then (-1) else 1)

instance  Num Frac where
     (+) (F x y) (F z w) = somafrac (F x y) (F z w)
     (-) (F x y) (F z w) = subfrac (F x y) (F z w)
     (*) (F x y) (F z w) = normaliza (F (x*z) (y*w))
     negate (F x y) = (F (-x) y)
     abs (F x y) = (F (abs x) (abs y))
     signum (F x y) = sinalfrac (normaliza (F x y))
     fromInteger x = (F x 1)



--2)

data ExpInt = Const Int
           | Simetrico (ExpInt)
           | Mais (ExpInt) (ExpInt)
           | Menos (ExpInt) (ExpInt)
           | Mult (ExpInt) (ExpInt)


--a)
calcula :: ExpInt -> String
calcula (Const a) = show a
calcula (Simetrico x) = "(" ++ "-" ++ calcula x ++ ")"
calcula (Mais x y) = "(" ++ calcula x ++ "+" ++ calcula y ++ ")"
calcula (Menos x y) = "(" ++ calcula x ++ "-" ++ calcula y ++ ")"
calcula (Mult x y) = "(" ++ calcula x ++ "*" ++ calcula y ++ ")"


instance Show (ExpInt) where
     show x = calcula x

--b)
instance Eq (ExpInt) where
     (==) x y = (calcula x) == (calcula y)


--c)

modulo :: ExpInt -> ExpInt
modulo (Simetrico x) = x

fazconta :: ExpInt -> Int
fazconta (Const x) = x
fazconta (Simetrico x) = -(fazconta x)
fazconta (Mais x y) = (fazconta x) + (fazconta y)
fazconta (Menos x y) = (fazconta x) - (fazconta y)
fazconta (Mult x y) = (fazconta x) * (fazconta y)

sinalExp :: ExpInt -> ExpInt
sinalExp x = if (fazconta x == 0) then (Const 0) else (if (fazconta x < 0) then (Const (-1)) else (Const 1))

instance Num (ExpInt) where
     (+) x y = (Mais x y)
     (-) x y = (Menos x y)      
     (*) x y = (Mult x y)
     abs x = modulo x
     signum x = sinalExp x
     fromInteger x = Const (fromInteger x)

--3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--a)

instance Eq Data where
     (==) (D d m a) (D d1 m1 a1) = (d==d1) && (m==m1) && (a==a1)  

instance Ord Data where
    compare (D d m a) (D d1 m1 a1) |(d==d1) && (m==m1) && (a==a1) = EQ --equal
                                   |(a1 > a) || (a==a1) && (m1>m) || (a==a1) && (m1==m) && (d1>d) = GT --greater then
                                   |otherwise = LT --lower then


--b)

data_print :: Data -> String
data_print (D x y z) = show x ++ "/" ++ show y ++ "/" ++ show z


instance Show Data where
     show x = data_print x


--c)

--ordena :: Extracto -> Extracto
--ordena (Ext f ((d,_,_):t)) 


--d)

e1= (Ext 100 [(D 10 11 2000,"putas",Debito 50),(D 5 12 1999,"salario",Credito 1500)])

print_extrato :: Extracto -> String
print_extrato (Ext x ((d,s,m):t)) = "Saldo anterior:" ++ show x++ "\n"



instance Show Extracto where
     show x = print_extrato x




















