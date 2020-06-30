module Main where

import Hangman
import Lib

main :: IO ()
main = do 
  word <-loadRandomWord
  let startGame = Game 0 word ""
  putStrLn $ render startGame 
  gameLoop startGame