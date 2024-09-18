module Practica03 where


import Data.List (nub)
-- 4.1 logica proposicional

data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving(Eq)

instance Show Prop where
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- Funciones Auxiliares
negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = Cons False
negar (Cons False) = Cons True
negar (Not f) = f
negar (And f1 f2) = Or (negar f1) (negar f2)
negar (Or f1 f2) = And (negar f1) (negar f2)
negar (Impl f1 f2) = And f1 (negar f2)
negar (Syss f1 f2)= negar (And (Impl f1 f2) (Impl f2 f1)) 

distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or q p)) (distribuir (Or r p))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p = p

--Función que eliminará elementos repetidos en una lista dada. 
eliminaRepetidos :: (Eq a) => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs)
    | x `elem` xs = eliminaRepetidos xs
    | otherwise = x : eliminaRepetidos xs

-- 4.2 Formas normales

-- Ejercicio 1: Implementar la funcion fnn que convierte una formula proposicional en su 
-- forma normal negativa.
fnn :: Prop -> Prop
fnn (Var x) = Var x
fnn (Not (Var x)) = Not (Var x)
fnn (Not (Not x)) = fnn x
fnn (Not (And f1 f2)) = Or (fnn (Not f1)) (fnn (Not f2))
fnn (Not (Or f1 f2)) = And (fnn (Not f1)) (fnn (Not f2))
fnn (Not (Impl f1 f2)) = And (fnn f1) (fnn (Not f2))
fnn (Not (Syss f1 f2)) = Syss (fnn (Not f1)) (fnn f2) 
fnn (And f1 f2) = And (fnn f1) (fnn f2)
fnn (Or f1 f2) = Or (fnn f1) (fnn f2)
fnn (Impl f1 f2) = Or (fnn (Not f1)) (fnn f2)
fnn (Syss f1 f2) = And (fnn (Impl f1 f2)) (fnn (Impl f2 f1)) 

-- Ejercicio 2: Implementar la funcion fnc, que convierte una formula proposicional en su forma normal conjuntiva.
-- Se recomienda usar la funcion fnn.
fnc :: Prop -> Prop
fnc (Or (And a b) (And c d)) = And (fnc (Or (And a b) c)) (fnc (Or (And a b) d))
fnc (Or a (And b c)) = And (fnc (Or a b)) (fnc (Or a c))
fnc (Or (And b c) a) = And (fnc (Or b a)) (fnc (Or c a))
fnc (Var a) = Var a
fnc (Not a) = distribuir (fnn (Not (fnc a)))
fnc (And a b) = distribuir (fnn (And (fnc a) (fnc b)))
fnc (Or a b) = distribuir (fnn (Or (fnc a) (fnc b)))
fnc (Impl a b) = distribuir (fnn (Or (Not (fnc a)) (fnc b)))
fnc (Syss a b) = distribuir (fnn (Syss (fnc a) (fnc b)))

-- 4.3 Resolucion binaria

-- Ejercicio 1: Crear un sinonimo Literal, que sera igual a Prop por simplicidad, aunque solo deberian ser variables 
-- o negaciones de variables.
type Literal = Prop

-- Ejercicio 2: Crear un sinonimo Clausula, que representara las clausulas como conjunto de literales.
type Clausula = [Literal]

-- Ejercicio 3: Definir la funcion clausulas que dada una formula en FNC, devuelve una lista con las clausulas que la forman.
clausulas :: Prop -> [Clausula]
clausulas (Var p) = [[Var p]]
clausulas (Not p) = [[Not p]]
clausulas (Cons p) = [[Cons p]]
clausulas (Or p q) = [clausula (Or p q)]
clausulas (And p q) = clausulas p ++ clausulas q

clausula :: Prop -> Clausula
clausula (Var p) = [Var p]
clausula (Cons p) = [Cons p]
clausula (Not (Var p)) = [Not (Var p)]
clausula (Or p q) = eliminaRepetidos (clausula p ++ clausula q)

-- Ejercicio 4: Definir la funcion resolucion que dadas dos clausulas, devuelve el resolvente obtenido despues de aplicar la 
-- regla de resolucion binaria. Se puede asumir que se puede obtener un resolvente a partir de los argumentos.

--Funcion auxiliar que elimina un elemento dado de una lista
elimina :: (Eq a) => a -> [a] -> [a]
elimina _ [] = []
elimina y (x:xs) = if y == x then xs else [x] ++ (elimina y xs)

--Funcion auxiliar que devuelve la resolucion pero con las literales repetidas
resolucionAux :: Clausula -> Clausula -> Clausula
resolucionAux [] ys = ys
resolucionAux ((Var p):xs) ys = if (Not (Var p)) `elem` ys
    then xs ++ (elimina((Not (Var p))) ys)
    else [Var p] ++ resolucionAux xs ys
resolucionAux ((Not (Var p)): xs) ys = if (Var p) `elem` ys
    then xs ++ (elimina (Var p)ys)
    else [Not (Var p)] ++ resolucionAux xs ys

--Función principal
resolucion :: Clausula -> Clausula -> Clausula
resolucion xs ys = eliminaRepetidos (resolucionAux xs ys)

-- 4.4 Algoritmo de saturacion

-- Ejercicio 1: Definir la funcion hayResolvente, que determina si es posible obtener un resolvente a partir de dos clausulas.
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = any (\l1 -> esNegacionDeAlguna l1 c2) c1
  where
    -- Comprueba si una literal es la negación de alguna en la cláusula dada
    esNegacionDeAlguna :: Literal -> Clausula -> Bool
    esNegacionDeAlguna l1 = any (\l2 -> esNegacion l1 l2)
    
    -- Comprueba si dos literales son negaciones entre sí
    esNegacion :: Literal -> Literal -> Bool
    esNegacion (Not p1) p2 = p1 == p2
    esNegacion p1 (Not p2) = p1 == p2
    esNegacion _ _ = False

--Ejercicio 2: Definir la funcion saturacion, que dada una formula proposicional, determina si esta es satisfacible o no 
--usando el algoritmo de saturacion.


-- Genera todos los resolventes posibles a partir de un conjunto de cláusulas
generarResolventes :: [Clausula] -> [Clausula]
generarResolventes [] = []
generarResolventes (c:cs) = nub (concatMap (resolucionCon c) cs ++ generarResolventes cs)
  where
    -- Esta función solo aplica resolución si hay un resolvente
    resolucionCon :: Clausula -> Clausula -> [Clausula]
    resolucionCon c cl
        | hayResolvente c cl = [resolucion c cl] ++ [resolucion cl c]
        | otherwise = []


-- Función que aplica la resolución hasta un límite
resn :: Int -> [Clausula] -> [Clausula]
resn 0 clauses = clauses
resn n clauses =
    let
        nuevosResolventes = generarResolventes clauses
        todasLasClausulas = nub (clauses ++ nuevosResolventes)
    in
        if any null todasLasClausulas  -- Si encontramos una cláusula vacía, hay contradicción
        then [ [] ] -- Devolver la cláusula vacía como señal de contradicción
        else if length todasLasClausulas == length clauses
             then todasLasClausulas  -- Si no hay cambios, devolvemos el conjunto de cláusulas
             else resn (n - 1) todasLasClausulas  -- Continuar aplicando resolución



-- Función de saturación
saturacion :: Prop -> Bool
saturacion formula = 
    let
        -- Convertir la fórmula a su forma normal conjuntiva (FNC)
        clauses = clausulas (fnc formula)
        
        -- Aplicar el algoritmo de saturación
        limite = 100  -- Ajustar este valor según sea necesario
        resultado = resn limite clauses
    in
        -- Comprobar si hay una cláusula vacía en el resultado (que indica contradicción)
        any null resultado
