module Main where

import Hangman
import Lib

main :: IO ()
main = do 
  loadRandomWord
  putStrLn $ render startGame 
  gameLoop startGame
  where startGame = Game 0 "guessMe" ""