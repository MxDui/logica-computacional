module Practica1 where

-- 4.1 Tipos de datos Algebraicos

-- Ejercicio 1: Definición del tipo de dato Shape y funciones asociadas
data Shape = Circle Float
           | Square Float
           | Rectangle Float Float
           | Triangle Float Float Float
           | Trapeze Float Float Float
           deriving (Show, Eq)

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Square l) = l^2
area (Rectangle w h) = w * h
area (Triangle a b c) = let s = (a + b + c) / 2
                        in sqrt (s * (s - a) * (s - b) * (s - c))
area (Trapeze b1 b2 h) = ((b1 + b2) / 2) * h

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Triangle a b c) = a + b + c
perimeter (Trapeze b1 b2 h) = b1 + b2 + (2 * sqrt(h^2 + ((b1 - b2) / 2)^2))

instance Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)

-- Ejercicio 2: Definición del tipo de dato Point y funciones asociadas
type Point = (Float, Float)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

fromO :: Point -> Float
fromO p = distance p (0, 0)

-- Ejercicio 3: Definición del tipo de dato Haskellium y funciones asociadas
data Haskellium = Haskellium {
    name :: String,
    lastName1 :: String,
    lastName2 :: String,
    location :: Point,
    houseShape :: Shape
} deriving (Show)

son :: Haskellium -> Haskellium -> String -> Haskellium
son h1 h2 n = Haskellium {
    name = n,
    lastName1 = lastName1 h1,
    lastName2 = lastName1 h2,
    location = location h1,
    houseShape = houseShape h1
}

houseCost :: Haskellium -> Float
houseCost h = let wallsArea = perimeter (houseShape h) * 2.5
                  roofArea = area (houseShape h)
              in wallsArea + roofArea

timeToWork :: Haskellium -> Float
timeToWork h = let dist = fromO (location h)
               in if dist < 300 then dist / 30 else dist / 70

-- 4.2 Listas y Funciones

-- Ejercicio 1: Implementación de la función isPal
isPal :: String -> Bool
isPal s = eliminaEspacios s == reverse (eliminaEspacios s)

-- función auxiliar para eliminar espacios de un string y poder comparar palíndromos
-- como la frase "anita lava la tina"
eliminaEspacios :: String -> String
eliminaEspacios [] = []
eliminaEspacios (x:xs) = if x == ' ' then eliminaEspacios xs else x : eliminaEspacios xs

-- Ejercicio 2: Implementación de la función concat'
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Ejercicio 3: Implementación de la función pascalN
pascalN :: Int -> [Int]
pascalN 0 = [1]
pascalN n = zipWith (+) (0 : pascalN (n-1)) (pascalN (n-1) ++ [0])

-- Ejercicio 4: Implementación de la función reversaFr usando foldr
reversaFr :: [a] -> [a]
reversaFr = foldr (\x acc -> acc ++ [x]) []
