module Tablero (Tablero,tableroVacio, colocarEnTablero, testEnTablero, printTablero, Ficha(..)) where

data Ficha = X | O deriving (Show)
data Casilla = Vacio | Ocupada Ficha

instance Show Casilla where
    show Vacio = "vacia"
    show (Ocupada ficha) = show ficha

type Fila = (Casilla, Casilla, Casilla)
type Tablero = (Fila, Fila, Fila)

filaVacia :: Fila
filaVacia = (Vacio, Vacio, Vacio)

tableroVacio :: Tablero
tableroVacio = (filaVacia, filaVacia, filaVacia)

colocarEnTablero :: (Int, Int) -> Ficha -> Tablero -> Tablero
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

testEnTablero :: (Int, Int) -> Tablero -> Bool
testEnTablero cord (f1, f2, f3) =
    case cord of
        (1, a) -> testEnFila a f1
        (2, a) -> testEnFila a f2
        (3, a) -> testEnFila a f3
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
testEnCasilla casillaOcupada = False

printTablero :: Tablero -> IO ()
printTablero (f1, f2, f3) = do
    printFila f1
    printFila f2
    printFila f3

printFila :: Fila -> IO ()
printFila (c1, c2, c3) = do
    putStrLn $ "\t"++ show c1 ++ "\t"++ show c2 ++ "\t"++ show c3 
