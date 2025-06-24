import Data.List
-- Ej1

-- max2 no esta currificada
-- max2 :: (Float, Float) -> Float
max2curry :: Float -> (Float -> Float)
max2curry x = (\y -> max2 y) 
    where 
        max2 y | x >= y = x
               | otherwise = y 

-- normaVectorial no esta currificada
-- normaVectorial :: (Float, Float) -> Float

normaVectorialCurry :: Float -> (Float -> Float)
normaVectorialCurry x = (\y -> sqrt (x^2 + y^2))

-- substract si esta currificada
substract :: Float -> (Float -> Float)
substract = flip (-)

-- predecesor esta curried
predecesor :: Float -> Float
predecesor = substract 1

-- evaluarEnCero esta curried
-- evaluarEnCero :: (Float -> a) -> a

-- dosVeces esta curried
--dosVeces :: (a -> b) -> a -> b
--dosVeces = \f -> f . f

-- flipAll ya esta cxurried
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

-- flipRaro ya esta curried
flipRaro :: b -> (a -> b -> c) -> a -> c 
flipRaro = flip flip

-- flipRaro 1 (-) 3
-- flip flip 1 (-) 3

----------------------------------------------

-- ej2
suma2 (x, y) = x + y

curry2 :: ((a, b) -> c) -> (a -> b -> c)
curry2 f x y = f (x, y)

uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f (x, y) = f x y

-- No es posible definir curryN porque no tenemos forma en haskell de hacer cosas sobre los tipos programaticamente

---------------------------------------------
--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f e [] = e
--foldr f e (x:xs) = fx (foldr f e xs)

-- ej3

-- i)

sum_foldr :: [Int] -> Int
sum_foldr = foldr (+) 0

elem_foldr :: (Eq a) => a -> [a] -> Bool
elem_foldr e = foldr ((||) . ( == e)) False

join_foldr :: [a] -> [a] -> [a]
join_foldr = flip (foldr (:))

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr f = foldr (\x rec -> if f x then (:) x rec else rec) []

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f = foldr (\x rec -> f x : rec ) []

-- ii)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

-- mejorSegun (>) [1,3,2]

-- f 1 (foldr1 f [3,2])
-- f 1 (f 3 (foldr1 [2]))
-- f 1 (f 3 2)
-- f 1 (3)
-- f 3

-- iii)
--foldl :: (b -> a -> b) -> b -> [a] -> b
--foldl f e [] = e
--foldl f e (x:xs) = foldl f (f e x) xs


sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\rec x -> rec ++ (if null rec then [x] else [x + last rec])) []

-- fold1
--sumarParciales [1,2,3]
-- fold1 f [] [1,2,3]
-- fold1 f (f [] 1) [2,3]
-- fold1 f ([1]) [2,3]
-- fold1 f (f [1] ++ 2) [3]
-- fold1 f ([1, 3]) [3]
-- fold1 f (f [1,3] 3) []
-- f [1, 3, 6] []
-- [1, 3, 6]

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

-- [1,2,3] -> 1 - 2 + 3
-- (-) 1 ( (-) 2 (-) 3 0)

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (flip (-)) 0


-- [1, 2, 3, 4] = 4 - (3 - (2 - (1 - 0)))
--              = 4 - 3 + 2 - 1 


-- Ej 5
-- i) No es recursion estructural porque a pesar de que el caso base devuelve un valor fijo
-- el caso no recursivo interactua con xs y no en el contexto del llamado recursivo de la funcion (if null xs / tail xs)


-- ii) es recursion estructural porque el caso base devuelve un valor fijo (la funcion identidad) y la recursion (xs) siempre es
-- usada en su totalidad en llamado recursivo (entrelazar xs)

-- la reescribiremos con foldr

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f e [] = e
--foldr f e (x:xs) = fx (foldr f e xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id

-- entrelazar [1,3] [2,4]
-- foldr f [] [1,3] [2,4]
-- f 1 (foldr f [] [3]) [2,4]
-- f 1 (f 3 (foldr f [] [])) [2, 4]
-- f 1 (f 3 []) [2, 4]

alternados :: [a] -> [a]
alternados lista = (foldr (\x rec bool -> if bool then x : rec False else rec True) (\_ -> []) lista) True

-- alternados [1,2,3,4] = [1,3]
-- foldr f [] [1,2,3,4]
-- f 1 (foldr f [] [2,3,4])
-- f 1 (f 2 (foldr f [] [3,4]))
-- f 1 (f 2 (f 3 (foldr f [] [4])))
-- f 1 (f 2 (f 3 (f 4 (foldr f [] []))))
-- f 1 (f 2 (f 3 (f 4 ([]))))


-- ej6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- a )
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x == e then xs else x : rec) []

-- b ) No es posible implementarla con foldr porque en cada iteracion, no tenemos acceso al resto de la lista que falta
-- procesar entonces al encontrar al elemento la primera vez, no podemos "terminar" y solo devolver el resto de la lista.

-- c )
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if x >= e then e : x : xs else x : rec) [e]


-- ej7

-- i)

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

-- ii)

armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

armarPares2 :: [a] -> [b] -> [(a,b)]
armarPares2 = foldr (\x rec ys -> if null ys then rec [] else (x, (head ys)) : rec (tail ys)) (const [])

-- iii)

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys -> if null ys then [] else (f x (head ys)) : rec (tail ys) ) (const [])

-- ej 9

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f e [] = e
--foldr f e (x:xs) = fx (foldr f e xs)

-- i)
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f e 0 = e
foldNat f e n = f n (foldNat f e (n-1))

-- ii)


potencia :: Integer -> Integer -> Integer
potencia x = foldNat (\_ rec -> rec * x) 1

-- potencia 2 3
-- foldNat f 1 3
-- f 3 (foldNat f 1 2)
-- f 3 (f 2 (foldNat f 1 1))
-- f 3 (f 2 (f 1 (foldNat f 1 0))
-- f 3 (f 2 (f 1 (1)))
-- f 3 (f 2 (2))
-- f 3 (4)
-- 8

-- ej 12

-- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
-- recr _ z [] = z
-- recr f z (x : xs) = f x xs (recr f z xs)

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

-- i)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB f e Nil = e
foldAB f e (Bin izq nodo der) = f (foldAB f e izq) nodo (foldAB f e der)

recAB :: (AB a -> AB a -> b -> a -> b -> b) -> b -> AB a -> b
recAB f e Nil = e
recAB f e (Bin izq nodo der) = f izq der (recAB f e izq) nodo (recAB f e der)

-- ii )
-- (Bin (Bin Nil 3 Nil) 5 Bin (Nil 1 (Bin Nil 4 Nil)))

esNil :: AB a -> Bool
esNil arbol = case arbol of
                Nil -> True
                Bin izq nodo der -> False

altura :: AB a -> Integer
altura = foldAB (\izq nodo der -> 1 + max izq der) 0

cantNodos :: AB a -> Integer
cantNodos = foldAB (\izq nodo der -> 1 + izq + der) 0

-- iii )

--mejorSegún :: (a -> a -> Bool) -> AB a -> a
--mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> (rl `g` v) `g` rr) (Bin l v r)
--    where g x y = if f x y then x else y

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin izq nodo der) = foldAB (\izq nodo der -> g nodo (g izq der)) nodo (Bin izq nodo der)
                where g x y = if f x y then x else y



-- Ej 15
--data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

--i)
data RoseTree a = Rose a [RoseTree a]

-- ii)

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f e [] = e
--foldr f e (x:xs) = fx (foldr f e xs)

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose n hijos) = f n (map (foldRose f) hijos)

-- iii)

hojas :: RoseTree a -> [a]
hojas = foldRose (\nodoActual hijos -> if null hijos then [nodoActual] else concat hijos)

-- (Rose 1 [ Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []]])

--foldRose f rt
-- f 1 (map (foldRose f) hijos)
-- f 1 ([foldRose f hijos2 , foldRose f hijos3])
-- f 1 ([f 2 (map (foldRose f) [])), f 3 (map (foldRose f) []))])
-- f 1 ([f 2 [], f 3 []])
-- f 1 ([[2], [3]])

distancias :: RoseTree a -> [Integer]
distancias = foldRose (\nodoActual hijos -> if null hijos then [0] else map (\n -> n + 1) (concat hijos))

alturaRT :: RoseTree a -> Integer
alturaRT = foldRose (\nodoActual hijos -> if null hijos then 1 else 1 + (mejorSegun (>) hijos))

--foldRose f rt
-- f 1 (map (foldRose f) hijos)
-- f 1 ([foldRose f hijos2 , foldRose f hijos3])
-- f 1 ([f 2 (map (foldRose f) [])), f 3 (map (foldRose f) []))])
-- f 1 ([f 2 [], f 3 []])
-- f 1 ([1, 1])


-- (Rose 10 [ Rose 20 [Rose 30 [Rose 40 []]], Rose 50 []])

-- (Rose 'A'[ Rose 'B' [ Rose 'C' [], Rose 'D' []], Rose 'E' [ Rose 'F' [ Rose 'G' [], Rose 'H' []]]])

--- Funciones utiles
--Funciones: foldr, foldl, foldr1, foldl1, map, zipWith, all, any, null, nub, sort, sortBy, mod, div, odd, even, (++), head, tail, init, last, length, replicate,
--repeat, iterate, filter, take, drop, elem, find, isNothing, isJust, fromJust, maybe, lookup, reverse, concat, union, intersect, (>>=), span, takeWhile, dropWhile,
--concatMap, and, or, sum, max, maximum, maximumBy, min, minimum, minimumBy, (==), (/=), compare, comparing, not, ord, chr


---------- repaso parcial --------------------
-- Recursion estructural (foldr)
-- Sea g :: [a] -> b definida por dos ecuaciones:

--              g [] = ⟨caso base⟩
--              g (x : xs) = ⟨caso recursivo⟩

-- Decimos que la definicion de g esta dada por recursion
-- estructural si:
-- 1. El caso base devuelve un valor z “fijo” (no depende de g).
-- 2. El caso recursivo no puede usar los parametros g ni xs,
-- salvo en la expresion (g xs):
--              g [] = z
--              g (x : xs) = . . . x . . . (g xs) . . .
--


-- Recursion primitiva (recr)
-- Sea g :: [a] -> b definida por dos ecuaciones:

--               g [] = ⟨caso base⟩
--              g (x : xs) = ⟨caso recursivo⟩

-- Decimos que la definicion de g esta dada por recursion primitiva
-- si:
-- 1. El caso base devuelve un valor z “fijo” (no depende de g).
-- 2. El caso recursivo no puede usar el parametro g,
-- salvo en la expresion (g xs).
-- (Sı puede usar el parametro xs).
--                        g [] = z
--                   g (x : xs) = . . . x . . . xs . . . (g xs) . . .


-- Recursion iterativa (foldl)
-- Sea g :: b -> [a] -> b definida por dos ecuaciones:

--            g ac [] = ⟨caso base⟩
--          g ac (x : xs) = ⟨caso recursivo⟩

-- Decimos que la definicion de g esta dada por recursion iterativa si:
-- 1. El caso base devuelve el acumulador ac.
-- 2. El caso recursivo invoca inmediatamente a (g ac’ xs),
-- donde ac’ es el acumulador actualizado en funcion de su
-- valor anterior y el valor de x.

---------------FIN TEORICA

data Buffer a = Empty | Write Int a (Buffer a) | Read Int (Buffer a) deriving(Show)
buf = Write 1 "a" $ Write 2 "b" $ Write 1 "c" $ Empty -- = Write 1 "a" (Write 2 "b" (Write 1 "c" (Empty)))
buf2 = Read 2 $ Write 1 "a" $ Write 2 "b" $ Write 1 "c" $ Empty -- = Write 1 "a" (Write 2 "b" (Write 1 "c" (Empty)))

foldBuffer :: b -> (Int -> a -> b -> b) -> (Int -> b -> b) -> Buffer a -> b
foldBuffer cEmpty cWrite cRead buffer = case buffer of
                                Empty -> cEmpty
                                Write pos cont buffer -> cWrite pos cont (rec buffer)
                                Read pos buffer -> cRead pos (rec buffer)
                where rec = foldBuffer cEmpty cWrite cRead

recBuffer :: b -> (Int -> a -> Buffer a -> b -> b) -> (Int -> Buffer a -> b -> b) -> Buffer a -> b
recBuffer cEmpty cWrite cRead buffer = case buffer of
                                Empty -> cEmpty
                                Write pos cont buffer -> cWrite pos cont buffer (rec buffer)
                                Read pos buffer -> cRead pos buffer (rec buffer)
                where rec = recBuffer cEmpty cWrite cRead


posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = foldBuffer ([]) (\pos _ rec -> union [pos] rec) (\pos rec -> filter (\i -> i /= pos) rec)

contenido :: Int -> Buffer a -> Maybe a
contenido i = foldBuffer (Nothing)
                       (\pos cont rec -> if i == pos then Just cont else rec)
                       (\pos rec -> if i == pos then Nothing else rec)

puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = recBuffer (True)
                         (\pos cont buffer rec -> rec)
                         (\pos buffer rec -> if (elem pos (posicionesOcupadas buffer)) then True && rec else False)

deshacer :: Buffer a -> Int -> Buffer a
deshacer buffer ops = (foldBuffer (\_ -> Empty)
                          (\pos cont rec ops -> if ops > 0 then rec (ops - 1) else Write pos cont (rec (ops - 1))) 
                          (\pos rec ops ->  if ops > 0 then (rec (ops - 1)) else Read pos (rec (ops - 1)))
                           buffer) ops


data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a) deriving (Show)

abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

foldABNV :: (a ->b) -> (a -> b -> b) -> (b -> a -> b -> b) -> ABNV a -> b
foldABNV cHoja cUni cBi arbol = case arbol of
                        Hoja x -> cHoja x
                        Uni nodo hijo -> cUni nodo (rec hijo)
                        Bi hijosI nodo hijosD -> cBi (rec hijosI) nodo (rec hijosD)
            where rec = foldABNV cHoja cUni cBi

recABNV :: (a -> b) -> (a -> ABNV a -> b -> b) -> (ABNV a -> ABNV a -> b -> a -> b -> b) -> ABNV a -> b
recABNV cHoja cUni cBi arbol = case arbol of
                        Hoja x -> cHoja x
                        Uni nodo hijo -> cUni nodo hijo (rec hijo)
                        Bi hijosI nodo hijosD -> cBi hijosI hijosD (rec hijosI) nodo (rec hijosD)
            where rec = recABNV cHoja cUni cBi


elemABNV :: Eq a => a -> ABNV a -> Bool
elemABNV e = foldABNV (\hoja -> hoja == e) (\nodo rec -> (nodo == e ) || rec) (\recI nodo recD -> (nodo == e) || recI || recD)


reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno x y = recABNV
                         (\nodo -> if nodo == x then Hoja y else Hoja nodo)
                         (\nodo hijos rec ->  if x == nodo then Uni y hijos else Uni nodo rec)
                         (\hijoI hijoD recI nodo recD -> if x == nodo then Bi hijoI y hijoD else (if elemABNV x hijoI then Bi recI nodo hijoD else Bi hijoI nodo recD))

nivel :: ABNV a -> Int -> [a]
nivel arbol n = foldABNV
            (\x actual -> if n == actual then [x] else [])
            (\nodo rec actual -> if n == actual then nodo : rec (actual + 1) else rec (actual + 1))
            (\recI nodo recD actual -> if n == actual then (recI (actual + 1)) ++ [nodo] ++ (recD (actual + 1)) else (recI (actual + 1)) ++ (recD (actual + 1))) arbol 0

