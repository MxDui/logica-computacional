module Main where

import Test.HUnit
import Practica02

-- Test cases for variables function
testVariables :: Test
testVariables = TestList [
    "Test 1" ~: variables (Var "p") ~?= ["p"],
    "Test 2" ~: variables (And (Var "p") (Var "q")) ~?= ["p", "q"],
    "Test 3" ~: variables (Not (Or (Var "p") (Var "q"))) ~?= ["p", "q"],
    "Test 4" ~: variables (Impl (Var "p") (Var "q")) ~?= ["p", "q"],
    "Test 5" ~: variables (Syss (Var "p") (Var "q")) ~?= ["p", "q"],
    "Test 6" ~: variables (And (Var "p") (And (Var "q") (Var "r"))) ~?= ["p", "q", "r"],
    "Test 7" ~: variables (Not (Not (Var "p"))) ~?= ["p"]
    ]

-- Test cases for conjPotencia function
testConjPotencia :: Test
testConjPotencia = TestList [
    "Test 1" ~: conjPotencia [1, 2] ~?= [[], [1], [2], [1, 2]],
    "Test 2" ~: conjPotencia "ab" ~?= ["", "a", "b", "ab"],
    "Test 3" ~: conjPotencia [1, 2, 3] ~?= [[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]],
    "Test 4" ~: conjPotencia "" ~?= [""]
    ]

-- Test cases for interpretacion function
testInterpretacion :: Test
testInterpretacion = TestList [
    "Test 1" ~: interpretacion (Var "p") ["p"] ~?= True,
    "Test 2" ~: interpretacion (Not (Var "p")) ["p"] ~?= False,
    "Test 3" ~: interpretacion (And (Var "p") (Var "q")) ["p", "q"] ~?= True,
    "Test 4" ~: interpretacion (Or (Var "p") (Var "q")) ["p"] ~?= True,
    "Test 5" ~: interpretacion (Impl (Var "p") (Var "q")) ["q"] ~?= True,
    "Test 6" ~: interpretacion (Syss (Var "p") (Var "q")) ["p", "q"] ~?= True,
    "Test 7" ~: interpretacion (Or (Var "p") (Var "q")) [] ~?= False,
    "Test 8" ~: interpretacion (And (Var "p") (Not (Var "q"))) ["p"] ~?= True
    ]

-- Test cases for estadosPosibles function
testEstadosPosibles :: Test
testEstadosPosibles = TestList [
    "Test 1" ~: estadosPosibles (Var "p") ~?= [["p"]],
    "Test 2" ~: estadosPosibles (And (Var "p") (Var "q")) ~?= [["p", "q"]],
    "Test 3" ~: estadosPosibles (Or (Var "p") (Var "q")) ~?= [["p"], ["q"]],
    "Test 4" ~: estadosPosibles (Not (Var "p")) ~?= [["p"]]
    ]

-- Test cases for tautologia function
testTautologia :: Test
testTautologia = TestList [
    "Test 1" ~: tautologia (Or (Var "p") (Not (Var "p"))) ~?= True,
    "Test 2" ~: tautologia (And (Var "p") (Not (Var "p"))) ~?= False,
    "Test 3" ~: tautologia (Impl (Var "p") (Var "p")) ~?= True,
    "Test 4" ~: tautologia (Syss (Var "p") (Var "p")) ~?= True
    ]

-- Test cases for contradiccion function
testContradiccion :: Test
testContradiccion = TestList [
    "Test 1" ~: contradiccion (And (Var "p") (Not (Var "p"))) ~?= True,
    "Test 2" ~: contradiccion (Or (Var "p") (Not (Var "p"))) ~?= False,
    "Test 3" ~: contradiccion (And (Var "p") (Var "p")) ~?= False,
    "Test 4" ~: contradiccion (Not (Or (Var "p") (Not (Var "p")))) ~?= True
    ]

-- Test cases for esModelo function
testEsModelo :: Test
testEsModelo = TestList [
    "Test 1" ~: esModelo (Var "p") ["p"] ~?= True,
    "Test 2" ~: esModelo (And (Var "p") (Var "q")) ["p", "q"] ~?= True,
    "Test 3" ~: esModelo (Or (Var "p") (Var "q")) ["p"] ~?= True,
    "Test 4" ~: esModelo (Not (Var "p")) [] ~?= True
    ]

-- Test cases for modelos function
testModelos :: Test
testModelos = TestList [
    "Test 1" ~: modelos (Var "p") ~?= [["p"]],
    "Test 2" ~: modelos (And (Var "p") (Var "q")) ~?= [["p", "q"]],
    "Test 3" ~: modelos (Or (Var "p") (Var "q")) ~?= [["p"], ["q"]],
    "Test 4" ~: modelos (Not (Var "p")) ~?= [[]]
    ]

-- Test cases for esValida function
testEsValida :: Test
testEsValida = TestList [
    "Test 1" ~: esValida (Or (Var "p") (Not (Var "p"))) ~?= True,
    "Test 2" ~: esValida (And (Var "p") (Not (Var "p"))) ~?= False,
    "Test 3" ~: esValida (Impl (Var "p") (Var "p")) ~?= True,
    "Test 4" ~: esValida (Syss (Var "p") (Var "p")) ~?= True
    ]

-- Test cases for esInsatisfacible function
testEsInsatisfacible :: Test
testEsInsatisfacible = TestList [
    "Test 1" ~: esInsatisfacible (And (Var "p") (Not (Var "p"))) ~?= True,
    "Test 2" ~: esInsatisfacible (Or (Var "p") (Not (Var "p"))) ~?= False,
    "Test 3" ~: esInsatisfacible (And (Var "p") (Var "p")) ~?= False,
    "Test 4" ~: esInsatisfacible (Not (Or (Var "p") (Not (Var "p")))) ~?= True
    ]

-- Test cases for esSatisfacible function
testEsSatisfacible :: Test
testEsSatisfacible = TestList [
    "Test 1" ~: esSatisfacible (Or (Var "p") (Not (Var "p"))) ~?= True,
    "Test 2" ~: esSatisfacible (And (Var "p") (Not (Var "p"))) ~?= False,
    "Test 3" ~: esSatisfacible (Or (Var "p") (Var "q")) ~?= True,
    "Test 4" ~: esSatisfacible (And (Var "p") (Var "q")) ~?= True
    ]

main :: IO Counts
main = runTestTT $ TestList [
    testVariables,
    testConjPotencia,
    testInterpretacion,
    testEstadosPosibles,
    testTautologia,
    testContradiccion,
    testEsModelo,
    testModelos,
    testEsValida,
    testEsInsatisfacible,
    testEsSatisfacible
    ]