module Juego (jugar) where

import Tablero (Ficha (..), Tablero, colocarEnTablero, printTablero, tableroVacio, testEnTablero)

data Estado = Terminado | EnCurso

type Partida = (Tablero, Estado)

jugar :: Int -> IO ()
jugar playerNum = do
    let tab = initTab
    case playerNum of
        1 -> juegaSolo tab
        2 -> juegaAcom tab
        _ -> return ()

juegaSolo :: Partida -> IO ()
juegaSolo = undefined

juegaAcom :: Partida -> IO ()
juegaAcom (_, Terminado) = return ()
juegaAcom (tab, EnCurso) = do
    putStrLn "Juega player X"
    (newtab, est) <- hacerMov (tab, EnCurso) X
    printTablero newtab
    putStrLn "Juega player O"
    (lasttab, lastest) <- hacerMov (newtab,est) O
    printTablero lasttab
    juegaAcom (lasttab, lastest)
    
    undefined

hacerMov :: Partida -> Ficha -> IO Partida
hacerMov (tab, Terminado) _ = return (tab, Terminado)
hacerMov (tab, est) ficha = do
    putStrLn "En que fila colocas tu pieza"
    fila <- getNum
    putStrLn "En que columna colocas tu pieza"
    columna <- getNum
    if testEnTablero (fila, columna) tab then return (colocarEnTablero (fila, columna) ficha tab, EnCurso) else hacerMov (tab,est) ficha

getNum :: IO Int
getNum = do
    putStrLn "Ingrese numero entre 1 y 3"
    num <- getLine
    if num == "1" || num == "2" || num == "3" then return (read num :: Int) else getNum

initTab :: Partida
initTab = (tableroVacio, EnCurso)
