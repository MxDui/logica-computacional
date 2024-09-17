import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
-- 4.1 logica proposicional

data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving(Eq,Ord)

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
fnn (Var p) = Var p
fnn (Cons p) = Cons p
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Not p) = negar p
fnn (Impl p q) = Or (negar p) (fnn q)
fnn (Syss p q) = And (fnn (Impl p q)) (fnn (Impl q p))

negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons p) = Cons (not p)
negar (Or p q) = And (negar p) (negar q)
negar (And p q) = Or (negar p) (negar q)
negar (Not p) = p
negar (Impl p q) = And p (negar q)
negar (Syss p q) = negar (And (Impl p q) (Impl q p))

-- Ejercicio 2: Implementar la funcion fnc, que convierte una formula proposicional en su forma normal conjuntiva.
-- Se recomienda usar la funcion fnn.
fnc :: Prop -> Prop
fnc (Var p) = Var p
fnc (Cons p) = Cons p
fnc (Or p q) = distribuir ( fnn (Or (distribuir (p)) (distribuir (q))))
fnc (And p q) = distribuir ( fnn (And (distribuir (p)) (distribuir (q))))
fnc (Not p) = distribuir ( fnn (Not (distribuir p)))
fnc (Impl p q) = distribuir ( fnn (Impl (distribuir p) (distribuir q)))
fnc (Syss p q) = distribuir ( fnn (Syss (distribuir p) (distribuir q)))

distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And p q) r) = And (distribuir (Or p r)) (distribuir (Or q r))
distribuir (And p (Or q r)) = distribuir (Or (distribuir (And p q)) (distribuir (And p r)))
distribuir (And (Or p q) r) = distribuir (Or (distribuir (And p r)) (distribuir (And q r)))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p = p

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
clausula (Or p q) = eliminarDuplicadosLiteral (clausula p ++ clausula q)

-- Ejercicio 4: Definir la funcion resolucion que dadas dos clausulas, devuelve el resolvente obtenido despues de aplicar la
-- regla de resolucion binaria. Se puede asumir que se puede obtener un resolvente a partir de los argumentos.
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 
  | null resolvent = []  -- Empty clause
  | otherwise = resolvent
  where
    resolvent = [l | l <- c1 ++ c2, not (complementary l `elem` c2) && not (complementary l `elem` c1)]
    complementary (Not (Var p)) = Var p
    complementary (Var p) = Not (Var p)
    complementary l = l

-- 4.4 Algoritmo de saturacion

-- Ejercicio 1: Definir la funcion hayResolvente, que determina si es posible obtener un resolvente a partir de dos clausulas.
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente [(Var p)] (ys) = (negar (Var p)) `elem` ys
hayResolvente [(Not (Var p))] (ys) = (Var p) `elem` ys
hayResolvente (xs) (ys) = Set.toList (Set.intersection (Set.fromList xs) (Set.fromList ys)) /= []
{-
hayResolvente (x:xs) (ys) = let resultado = hayResolvente [x] ys
                            in
                              if resultado 
                              then resultado 
                              else (hayResolvente xs ys)
-}

--Ejercicio 2: Definir la funcion saturacion que dada una formula proposicional, determina si esta es satisfacible o no 
--usando el algoritmo de saturacion.
saturacion :: Prop -> Bool
saturacion p = not (saturacionC (clausulas (fnc p)))

saturacionC :: [Clausula] -> Bool
saturacionC clausulas = aux clausulas
  where
    aux cls
      | [] `elem` cls = True  -- Empty clause found, formula is unsatisfiable
      | newCls == cls = False  -- No new clauses generated, formula is satisfiable
      | otherwise = aux newCls
      where newCls = nub (cls ++ [resolucion c1 c2 | c1 <- cls, c2 <- cls, c1 /= c2, hayResolvente c1 c2])

igualdad :: [Clausula] -> [Clausula] -> Bool
igualdad xs ys = ((Set.fromList (xs)) `Set.isSubsetOf`(Set.fromList (ys))) && ((Set.fromList (ys)) `Set.isSubsetOf` (Set.fromList (xs)))

listaResolventes :: [Clausula] -> [Clausula]
listaResolventes [] = []
listaResolventes [clausula] = [clausula]
listaResolventes (x:xs) = eliminarDuplicadosClausula (xs ++ listaResolvente x xs ++ [x])

listaResolvente :: Clausula -> [Clausula] -> [Clausula]
listaResolvente clausulas [] = []
listaResolvente clausula (y:ys) = if (hayResolvente clausula y)
                                  then  ((listaResolvente clausula ys) ++ [(resolucion clausula y)])
                                  else  ((listaResolvente clausula ys))

prueba :: IO ()
prueba = do
     let prop = (And (And (Or (p) (r)) (Or ((Not p)) ((Not r)))) (And (p) (r)))
         clau = clausulas prop
         resolv1 = listaResolventes clau
         resolv2 = listaResolventes resolv1
         resolv3 = listaResolventes resolv2
         resolv4 = listaResolventes resolv3
         resolv5 = listaResolventes resolv4
         resolv6 = listaResolventes resolv5
         resolv7 = listaResolventes resolv6
      
     putStrLn $ show prop
     putStrLn $ show clau
     putStrLn $ "1." ++ show resolv1
     putStrLn $ "2." ++ show resolv2
     putStrLn $ "3." ++ show resolv3
     putStrLn $ "4." ++ show resolv4
     putStrLn $ "5." ++ show resolv5
     putStrLn $ "6." ++ show resolv6
     putStrLn $ "7." ++ show resolv7
     
     putStrLn $ "--------------------------------------------"

     let lista = resolv5
     let clausulas = resolv6

     putStrLn $ "Clausula vacia"
     if ([(Var "Clausula vacia")]) `elem` lista
     then putStrLn $ show True
     else putStrLn $ show False

     putStrLn $ "igualdad"
     if (igualdad clausulas lista)
     then putStrLn $ "Son iguales"
     else putStrLn $ "no son iguales"

main :: IO ()
main = prueba

eliminarDuplicadosLiteral :: [Literal] -> Clausula
eliminarDuplicadosLiteral = nub

eliminarDuplicadosClausula :: [Clausula] -> [Clausula]
eliminarDuplicadosClausula = nub