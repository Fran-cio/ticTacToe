module Main where

import Juego (jugar)

main :: IO ()
main = do
    putStrLn "Welcome to Ta te ti"
    playerNum <- numberOfPlayers
    launchGame playerNum 
    putStrLn "Fin del Juego"

numberOfPlayers :: IO Int
numberOfPlayers = do
    putStrLn "Ingrese el numero de Jugadores (1 o 2)"
    playerNum <- getLine
    if playerNum == "1" || playerNum == "2" then return (read playerNum :: Int) else numberOfPlayers

launchGame :: Int -> IO ()
launchGame playerNum = do
  jugar playerNum
  return()
