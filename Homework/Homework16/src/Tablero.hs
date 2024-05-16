{-# LANGUAGE InstanceSigs #-}
module Tablero (Tablero,tableroVacio, colocarEnTablero, testEnTablero, printTablero, Ficha(..), Casilla(..)) where

data Ficha = X | O deriving (Show)
data Casilla = Vacio | Ocupada Ficha

instance Show Casilla where
    show :: Casilla -> String
    show Vacio = " "
    show (Ocupada ficha) = show ficha

type Fila = (Casilla, Casilla, Casilla)
type Tablero = (Fila, Fila, Fila)

type Coordenada = (Int, Int)

filaVacia :: Fila
filaVacia = (Vacio, Vacio, Vacio)

tableroVacio :: Tablero
tableroVacio = (filaVacia, filaVacia, filaVacia)

colocarEnTablero :: Coordenada -> Ficha -> Tablero -> Tablero
colocarEnTablero cord ficha (f1, f2, f3) =
    case cord of
        (1, a) -> (colocarEnFila a ficha f1, f2, f3)
        (2, a) -> (f1, colocarEnFila a ficha f2, f3)
        (3, a) -> (f1, f2, colocarEnFila a ficha f3)
        _ -> (f1, f2, f3)

colocarEnFila :: Int -> Ficha -> Fila -> Fila
colocarEnFila fila ficha (c1, c2, c3) =
    case fila of
        1 -> (colocarEnCasilla ficha c1, c2, c3)
        2 -> (c1, colocarEnCasilla ficha c2, c3)
        3 -> (c1, c2, colocarEnCasilla ficha c3)
        _ -> (c1, c2, c3)

colocarEnCasilla :: Ficha -> Casilla -> Casilla
colocarEnCasilla ficha Vacio = Ocupada ficha
colocarEnCasilla _ casillaOcupada = casillaOcupada

testEnTablero :: Coordenada -> Tablero -> Bool
testEnTablero cord (f1, f2, f3) =
    case cord of
        (1, col) -> testEnFila col f1
        (2, col) -> testEnFila col f2
        (3, col) -> testEnFila col f3
        _ -> False

testEnFila :: Int -> Fila -> Bool
testEnFila fila (c1, c2, c3) =
    case fila of
        1 -> testEnCasilla c1
        2 -> testEnCasilla c2
        3 -> testEnCasilla c3
        _ -> False

testEnCasilla ::  Casilla -> Bool
testEnCasilla Vacio = True
testEnCasilla _ = False

printTablero :: Tablero -> IO ()
printTablero (f1, f2, f3) = do
    putStrLn "\t    1   2   3 "
    putStrLn "\t  ┌───┬───┬───┐ "
    printFila 1 f1
    putStrLn "\t  ├───┼───┼───┤ "
    printFila 2 f2
    putStrLn "\t  ├───┼───┼───┤ "
    printFila 3 f3
    putStrLn "\t  └───┴───┴───┘ "
    putStrLn "────────────────────────────────────"

printFila :: Int -> Fila -> IO ()
printFila n (c1, c2, c3) = do
    putStr $ "\t" ++ show n ++ " "
    putStr $ "│ " ++ show c1 ++ " │ " ++ show c2 ++ " │ " ++ show c3 ++ " │ "
    putStrLn ""
