
-- 3.1 Sintaxis de la logica proposicional

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

-- 3.2 Ejercicios a implementar.

--Ejercicio 1: Definir la funcion variables de f que devuelve el conjunto formado por todas las variables proposicionales que aparecen en f.
variables :: Prop -> [String]
variables (Cons b) = []
variables (Var p) = [p]
variables (Not p) = variables p
variables (And p q) = eliminarDuplicados (variables p ++ variables q)
variables (Or p q) = eliminarDuplicados (variables p ++ variables q)
variables (Impl p q) = eliminarDuplicados (variables p ++ variables q)
variables (Syss p q) = eliminarDuplicados (variables p ++ variables q)

eliminarDuplicados :: [String] -> [String]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = x : eliminarDuplicados(filter (/= x) xs)

--Ejercicio 2: Definir la funcion conjPotencia x devuelve la lista de todos los subconjuntos de x.
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

--Ejercicio 3: Definir la funcion interpretacion f i es la interpretacion de f bajo i.
interpretacion:: Prop -> Estado -> Bool
interpretacion (Var p) xs = p `elem` xs
interpretacion (Cons b) xs = b
interpretacion (Not p) xs = not (interpretacion p xs)
interpretacion (And p q) xs = (interpretacion p xs) && (interpretacion q xs)
interpretacion (Or p q) xs = (interpretacion p xs) || (interpretacion q xs)
    -- para la implicacion y doble implicacion usar equivalencias 
interpretacion (Impl p q) xs = (interpretacion (Not p) xs) || (interpretacion q xs)
interpretacion (Syss p q) xs = (interpretacion (Impl p q) xs) && (interpretacion (Impl q p) xs)

--Ejercicio 4: Definir la funcion estadosPosibles devuelve todos los estados con los que podemos evaluar la formula.
estadosPosibles :: Prop -> [[String]]
estadosPosibles p = [[x | x <- xs] | xs <- conjPotencia (variables p), (interpretacion p xs) == True]

--Ejercicio 5: Definir la funcion tautologia que nos diga si es una tautologia.

--Ejercicio 6: Definir la funcion contradiccion que nos diga si es una contradiccion.

--Ejercicio 7: Definir la funcion esModelo que verifique si una interpretacion es un modelo.

--Ejercicio 8: Definir la funcion modelos que devuelve la lista de todos sus modelos.

--Ejercicio 9: Definir la funcion esValida que verifica si una formula proposicional es valida o no.

--Ejercicio 10: Definir la funcion esInsatisfacible verifica si una formula proposicional es insatisfacible.

--Ejercicio 11: Definir la funcion esSatisfacible verifica si una formula proposicional es satisfacible.