module Juego (jugar) where

import Tablero (Casilla (..), Ficha (..), Tablero, colocarEnTablero, printTablero, tableroVacio, testEnTablero)
import System.Random (randomRIO)

data Estado = Terminado String | EnCurso

instance Semigroup Estado where
    EnCurso <> e = e
    e <> EnCurso = e
    Terminado s1 <> Terminado s2 = Terminado (s1 ++ ", " ++ s2)

type Partida = (Tablero, Estado)

initTab :: Partida
initTab = (tableroVacio, EnCurso)

jugar :: Int -> IO ()
jugar playerNum = do
    let tab = initTab
    printTablero (fst tab)
    case playerNum of
        1 -> juegaSolo tab
        2 -> juegaAcom tab
        _ -> return ()

juegaSolo :: Partida -> IO ()
juegaSolo (_, Terminado a) =do
  putStrLn a
  return ()
juegaSolo (tab, EnCurso) = do
    putStrLn "Player with X plays."
    (newtab, est) <- hacerMov (tab, EnCurso) X
    printTablero newtab
    putStrLn "The AI plays with O."
    (lasttab, lastest) <- hacerMovIA (newtab, est) O
    printTablero lasttab
    juegaSolo (lasttab, lastest)
    return()


hacerMovIA :: Partida -> Ficha -> IO Partida
hacerMovIA (tab, Terminado a) _ = return (tab, Terminado a)
hacerMovIA (tab, est) ficha = do
    columna <- randomRIO (1, 3)
    fila <- randomRIO (1, 3)
    let valid = testEnTablero (columna, fila) tab
    if valid then do
      let newTab =colocarEnTablero (columna, fila) ficha tab
      return (newTab, finDePartida newTab)
    else do
     hacerMovIA (tab, est) ficha



juegaAcom :: Partida -> IO ()
juegaAcom (_, Terminado a) = do
  putStrLn a
  return ()
juegaAcom (tab, EnCurso) = do
    putStrLn "Player with X plays."
    (newtab, est) <- hacerMov (tab, EnCurso) X
    printTablero newtab
    putStrLn "Player with O plays."
    (lasttab, lastest) <- hacerMov (newtab, est) O
    printTablero lasttab
    juegaAcom (lasttab, lastest)
    return()


hacerMov :: Partida -> Ficha -> IO Partida
hacerMov (tab, Terminado a) _ = return (tab, Terminado a)
hacerMov (tab, est) ficha = do
    putStrLn "Which column do you place your piece in?"
    columna <- getNum
    putStrLn "Which row do you place your piece in?"
    fila <- getNum
    let cord = (fila, columna)
    let valid = testEnTablero cord tab
    if valid then do
      let newTab =colocarEnTablero cord ficha tab
      return (newTab, finDePartida newTab)
    else do
     putStrLn "That square is already occupied."
     hacerMov (tab, est) ficha

getNum :: IO Int
getNum = do
    putStrLn "Enter a number between 1 and 3."
    num <- getLine
    if num == "1" || num == "2" || num == "3" then return (read num :: Int) else getNum

finDePartida :: Tablero -> Estado
finDePartida tab = algunaDiag tab <> algunaFila tab <> algunaCol tab <> checkEmpate tab

algunaCol :: Tablero -> Estado
algunaCol ((c11, c21, c31), (c12, c22, c32), (c13, c23, c33)) = checkTriple [c11, c12, c13] <> checkTriple [c21, c22, c23] <> checkTriple [c31, c32, c33]

algunaFila :: Tablero -> Estado
algunaFila ((f11, f12, f13), (f21, f22, f23), (f31, f32, f33)) = checkTriple [f11, f12, f13] <> checkTriple [f21, f22, f23] <> checkTriple [f31, f32, f33]

algunaDiag :: Tablero -> Estado
algunaDiag ((d11, _, d21), (_, d12_22, _), (d23, _, d13)) = checkTriple [d11, d12_22, d13] <> checkTriple [d21, d12_22, d23]

checkTriple :: [Casilla] -> Estado
checkTriple [Ocupada X, Ocupada X, Ocupada X] = Terminado "The winner is X"
checkTriple [Ocupada O, Ocupada O, Ocupada O] = Terminado "The winner is O"
checkTriple _ = EnCurso

checkEmpate :: Tablero -> Estado
checkEmpate ((Ocupada _, Ocupada _, Ocupada _), (Ocupada _, Ocupada _, Ocupada _), (Ocupada _, Ocupada _, Ocupada _)) = Terminado "The game ended in a draw."
checkEmpate _ = EnCurso
