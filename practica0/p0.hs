-- Ej1 --

-- null
-- [a] -> bool
-- devuelve verdadero si la lista a esta vacia

-- head
-- [a] -> a
-- devuelve el primer elemento de la lista

-- tail
-- [a] -> [a]
-- devuelve la lista sin su primer elemento

-- init
-- [a] -> [a]
-- devulve la lista sin su ultimo element

-- last
-- [a] -> a
-- devuelve el ultimo elemento de la lista

-- take
-- [a] -> b -> [a]
-- devuelve una lista con los primeros b elementos de la lista original

-- drop
-- [a] -> b -> [a]
-- devuelve una lista con los ultimos b elementos de la lista original

-- (++)
-- [a] -> [a] -> [a]
-- devuelve la segunda lista unida al final de la primera

-- concat
-- [[a]] -> [a]
-- devuelve una lista con los elementos de todas las listas originales

-- reverse
-- [a] -> [a]
-- devuelve la lista original con sus elementos en el orden inverso

-- elem
-- [a] -> a -> Bool
-- devuelve verdadero si el elemento a pertenece a la lista original



---------------------------------------------

-- Ej2 --

valorAbsoluto :: Float -> Float
valorAbsoluto n | n > 0 = n
                | otherwise = n * (-1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n (n-1)

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n 1 = True
esPrimoAux n a | mod n a == 0 = False
               | otherwise = esPrimoAux n (a-1)
               
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantDivisoresPrimosAux n (n-1) 0

cantDivisoresPrimosAux :: Int -> Int -> Int -> Int
cantDivisoresPrimosAux n 1 z = z
cantDivisoresPrimosAux n a z | mod n a == 0 && esPrimo a = cantDivisoresPrimosAux n (a-1) (z+1)
                             | otherwise = cantDivisoresPrimosAux n (a-1) z


-- Ej3 --

inverso :: Float -> Maybe Float
inverso n | n /= 0 = Just (1 / n)
          | otherwise = Nothing

aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right n) | n == True = 1
                  | otherwise = 0


-- Ej4 --

limpiar :: String -> String -> String
limpiar x [] = []
limpiar x (y:ys) | elem y x = limpiar x ys
                 | otherwise = y : limpiar x ys


todosIguales :: [Int] -> Bool
todosIguales (x:[]) = True
todosIguales (x:y:xs) | x == y = True && todosIguales xs
                      | otherwise = False

-- Ej5 --

data AB a = Nil | Bin (AB a) a (AB a) deriving(Show)

vacio :: AB a -> Bool
vacio Nil = True
vacio n = False

negar :: Bool -> Bool
negar False = True
negar True = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq val der) = Bin (negacionAB izq) (negar val) (negacionAB der) 

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq val der) = val * productoAB izq * productoAB der