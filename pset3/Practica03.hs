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

-- 4.2 Formas normales

-- Ejercicio 1: Implementar la funcion fnn que convierte una formula proposicional en su forma normal negativa.
fnn :: Prop -> Prop

-- Ejercicio 2: Implementar la funcion fnc, que convierte una formula proposicional en su forma normal conjuntiva.
-- Se recomienda usar la funcion fnn.
fnc :: Prop -> Prop

-- 4.3 Resolucion binaria

-- Ejercicio 1: Crear un sinonimo Literal, que sera igual a Prop por simplicidad, aunque solo deberian ser variables 
-- o negaciones de variables.
type Literal = Prop

-- Ejercicio 2: Crear un sinonimo Clausula, que representara las clausulas como conjunto de literales.
type Clausula = [Literal]

-- Ejercicio 3: Definir la funcion clausulas que dada una formula en FNC, devuelve una lista con las clausulas que la forman.
clausulas :: Prop -> [Clausula]

-- Ejercicio 4: Definir la funcion resolucion que dadas dos clausulas, devuelve el resolvente obtenido despues de aplicar la 
-- regla de resolucion binaria. Se puede asumir que se puede obtener un resolvente a partir de los argumentos.
resolucion :: Clausula -> Clausula -> Clausula

-- 4.4 Algoritmo de saturacion

-- Ejercicio 1: Definir la funcion hayResolvente, que determina si es posible obtener un resolvente a partir de dos clausulas.
hayResolvente :: Clausula → Clausula → Bool

--Ejercicio 2: Definir la funcion saturacion, que dada una formula proposicional, determina si esta es satisfacible o no 
--usando el algoritmo de saturacion.
saturacion :: Prop → Bool
