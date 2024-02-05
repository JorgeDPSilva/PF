 --1 a)
--ficha 10
import System.Random
import Data.List

soma :: Int -> Int -> Int
soma a b = a + b 

main :: IO ()
main = do putStrLn "Insira o primeiro numero"
          a <- getLine
          putStrLn "Insira o segundo numero"
          b <- getLine
          putStrLn ("A soma de " ++ a ++ " e " ++ b ++ " é " ++ show (soma (read a) (read b)))

bingo :: IO ()
bingo = bingoAux 1
  where
    bingoAux :: Int -> IO ()
    bingoAux 91 = putStrLn "Terminei"
    bingoAux n = do _ <- getChar
                    x <- randomRIO (1::Int,90)
                    putStrLn $ "Gerei " ++ show x
                    bingoAux (n+1)


geraNaoRepetido :: [Int] -> IO Int
geraNaoRepetido l = do x <- randomRIO (1,9)
                       if elem x l then geraNaoRepetido l
                       else return x




bingo' :: IO ()
bingo' = bingoAux' []
  where
    bingoAux' :: [Int] -> IO ()
    bingoAux' l = do _ <- getChar
                     x <- geraNaoRepetido l 
                     putStrLn $ "Gerei " ++  show x
                     if length l == 8 then putStrLn "Terminei "
                        else bingoAux' (x:l)


bingo'' :: IO ()
bingo'' = bingoAux [1..9]
     where bingoAux :: [Int] -> IO ()
           bingoAux [] = putStrLn "Terminei"
           bingoAux l = do _ <- getChar
                           x <- randomRIO (0, length l -1)
                           putStrLn $ "Gerei " ++ show (l!!x)
                           bingoAux (delete (l!!x) l)


mastermind :: IO ()
mastermind = do 
    x1 <- randomRIO ('0','9')
    x2 <- randomRIO ('0','9')
    x3 <- randomRIO ('0','9')
    x4 <- randomRIO ('0','9')
    adivinha [x1,x2,x3,x4] 
       where 
          adivinha :: String -> IO ()
          adivinha chave = do 
            jogada <- getLine 
            let (posicaoCorreta, posicaoErrada) = compara jogada chave
            putStrLn $ "Posicao correta: " ++ show posicaoCorreta ++ "\nPosicao errada: " ++ show posicaoErrada
            
            if posicaoCorreta == 4 then putStrLn "Ganhou!!"
                else adivinha chave

compara :: String -> String -> (Int,Int)
compara jogada chave = (c,a)
   where c = length $ filter (\x -> x == True) (zipWith (==) jogada chave)  




    