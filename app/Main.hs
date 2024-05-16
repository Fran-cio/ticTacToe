module Main where

import Juego (jugar)

main :: IO ()
main = do
    putStrLn "Welcome to Tic tac toe"
    playerNum <- numberOfPlayers
    launchGame playerNum 
    putStrLn "Good Bye :)"

numberOfPlayers :: IO Int
numberOfPlayers = do
    putStrLn "Please enter the number of players (1 or 2)."
    playerNum <- getLine
    if playerNum == "1" || playerNum == "2" then return (read playerNum :: Int) else numberOfPlayers

launchGame :: Int -> IO ()
launchGame playerNum = do
  jugar playerNum
  return()
