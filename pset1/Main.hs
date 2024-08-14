module Main where

import Practica1

main :: IO ()
main = do
    -- Testing Shape functions
    let circle = Circle 5.0
    let square = Square 4.0
    let rectangle = Rectangle 4.0 6.0
    let triangle = Triangle 3.0 4.0 5.0
    let trapeze = Trapeze 5.0 3.0 4.0

    putStrLn $ "Area of Circle: " ++ show (area circle)
    putStrLn $ "Perimeter of Square: " ++ show (perimeter square)
    putStrLn $ "Comparison of Circle and Square by area: " ++ show (circle > square)
    
    -- Testing Point functions
    let p1 = (0.0, 0.0)
    let p2 = (3.0, 4.0)
    
    putStrLn $ "Distance between points: " ++ show (distance p1 p2)
    putStrLn $ "Distance from origin: " ++ show (fromO p2)
    
    -- Testing Haskellium functions
    let haskellium1 = Haskellium "John" "Doe" "Smith" p1 circle
    let haskellium2 = Haskellium "Jane" "Doe" "Brown" p2 square
    
    let child = son haskellium1 haskellium2 "Baby"
    
    putStrLn $ "Child's Name: " ++ name child
    putStrLn $ "Child's LastName1: " ++ lastName1 child
    putStrLn $ "Child's LastName2: " ++ lastName2 child
    
    putStrLn $ "Cost to build the house: " ++ show (houseCost haskellium1)
    putStrLn $ "Time to work (Haskellium1): " ++ show (timeToWork haskellium1)
    putStrLn $ "Time to work (Haskellium2): " ++ show (timeToWork haskellium2)
    
    -- Testing List and Function exercises
    putStrLn $ "Is 'madam' a palindrome? " ++ show (isPal "madam")
    putStrLn $ "Concatenation of [[1,2,3],[4,5,6]]: " ++ show (concat' [[1,2,3],[4,5,6]])
    putStrLn $ "3rd row of Pascal's Triangle: " ++ show (pascalN 3)
    putStrLn $ "Reversal of [1,2,3,4]: " ++ show (reversaFr [1,2,3,4])
