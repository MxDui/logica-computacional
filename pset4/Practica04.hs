module Practica03 where

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

type Literal = Prop

type Clausula = [Literal]

type Interpretacion = [(String, Bool)]

type Estado = (Interpretacion, [Clausula])

-- 4.2. Implementación de algoritmo DPLL (Parte 1)

{-Ejercicio 1: Determina si la claúsula vacía, representada como una lista vacía, forma parte del conjunto 
de claúsulas. Puesto que no existe modelo que satisfaga a la claúsula vacía, la búsqueda del
modelo falla.-}
conflict :: Estado -> Bool
conflict = error "No implementado"

{-Ejecicio 2: Determina si la búsqueda del modelo ha sido exitosa, esto sucede cuando el conjunto de cláusulas
es vacío.-}
success :: Estado -> Bool
success = error "No implementado"

{-Ejecicio 3: Si l es una literal que pertenece a una claúsula unitaria, entonces basta con agregar l al
modelo y seguir con la búsqueda. Es importante notar que si lcom ∈ M esta regla no puede aplicarse, 
de lo contrario se necesitaria que tanto l como lcom sean verdaderas, lo que conduce a una contradicción.
Nota: Para esta regla solo aplica la primer cláusula unitaria encontrada -}
unit :: Estado -> Estado
unit = error "No implementado"

{-Ejecicio 4: Si l es una literal que pertenece al modelo M y se tiene la claúsula l ∨ C entonces, dado
que l es verdadera, l ∨ C también lo es, por lo que se elimina la claúsula l ∨ C del conjunto
de cláusulas.
Nota: Esta regla se aplica para cada una de las literales en la interpretación -}
elim :: Estado -> Estado
elim = error "No implementado"

{-Ejecicio 5: Si l es una literal que pertenece al modelo M y se tiene la claúsula lcom ∨ C entonces, dado
que l es verdadera, lcom es falsa, por lo que solo es de interés saber si C es satisfacible.
Nota: Esta regla se aplica para cada una de las literales en la interpretación-}
red :: Estado ->Estado
red = error "No implementado"

{-Ejecicio 6: Dada una literal l se procede a buscar que M, l sea modelo de F, o que M, lcom lo sea.-}
sep :: Literal ->Estado ->(Estado, Estado)
sep = error "No implementado"

-- 4.3.Árboles DPLL

data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void

-- 4.4.Implementación de algoritmo DPLL (Parte 2)

{-Ejecicio 1: Dada una fórmula proposicional nos regresa la literal que más apariciones tiene en la fórmula. -}
heuristicsLiteral :: [Clausula] ->Literal
heuristicsLiteral = error "No implementado"

{-Ejecicio 2: Que recibe una fórmula proposicional en forma clausular y devuelve una interpretación que
satisfaga a la fórmula obtenida mediante la ejecución del algoritmo DPLL iniciando la ejecución
con el estado ∅ |= F. En caso de que la fórmula no sea satisfacible, la función deberá devolver
una lista vacía. Es fundamental recordar que el algoritmo trabaja con las cláusulas de una
fórmula proposicional en forma normal conjuntiva.-}
dpll :: [Clausula] -> Interpretacion
dpll = error "No implementado"


