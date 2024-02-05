import Data.List

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


-- Exercicio 1

testahoras :: (Int,Int) -> (Int, Int) -> Bool
testahoras (h1,h2) (h3,h4) = if (h1<=23 && h1>= 0 && h3<=23 && h3>=0 && h2<=59 && h2>=0 && h4<=59 && h4>=0 ) then True else False

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H x y) (H w z) = ((w-x)*60 +(z-y))  -- minutos total de uma etapa

diferencaEtapa :: Viagem -> Int
diferencaEtapa [] = 0
diferencaEtapa (((H x y),(H x1 y1)):t) = (diferencaHoras (H x y) (H x1 y1)) + diferencaEtapa t


-- a)
testaetapa :: Etapa -> Bool
testaetapa ((H h1 h2),(H h3 h4)) = if ((h1<h3) || (h1==h3 && h2<h4) && testahoras(h1,h2)(h3,h4) ) then True else False 

-- b)
testaViagem :: Viagem -> Bool 
testaViagem [] = True
testaViagem (((H x y),(H w z)):[]) = (testaetapa ((H x y), (H w z))) 
testaViagem(((H h1 h2),(H h3 h4)):((H h5 h6),(H h7 h8)):t) = (testaetapa ((H h1 h2),(H h3 h4))) && (testaetapa((H h3 h4),(H h5 h6)))  && testaViagem (((H h5 h6),(H h7 h8)):t)

-- c)
horaPartidaChegada :: Viagem -> (Hora,Hora)
horaPartidaChegada (((H x y),(H w z)):[]) = (H x y,H w z)
horaPartidaChegada (((H x y),(H w z)):((H x1 y1),(H w1 z1)):[]) = (H x y, H w1 z1)
horaPartidaChegada (((H x y),(H w z)):((H x1 y1),(H w1 z1)):t)= horaPartidaChegada(((H x y),(H w z)):t)


-- d)
tempoTotal :: Viagem -> Int
tempoTotal [] = 0
tempoTotal [((H x y),(H x1 y1))] = diferencaHoras (H x y) (H x1 y1)
tempoTotal l = diferencaEtapa l + tempoespera l


-- e)
tempoespera :: Viagem -> Int
tempoespera [] = 0
tempoespera (((H x y),(H w z)):[]) = 0
tempoespera (((H x y),(H w z)):((H x1 y1),(H w1 z1)):t) = (diferencaHoras(H w z) (H x1 y1)) + tempoespera (((H x1 y1),(H w1 z1)):t)