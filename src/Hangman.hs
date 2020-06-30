module Hangman where

import System.IO ( stdout, hFlush, hFlush )
import Data.Char (toUpper, toLower)
import System.Random

type Fails = Int

data Game = Game {
  fails :: Fails,
  toGuess :: String,
  guessedChars :: String
}

isWon ::  Game -> Bool
isWon g = all (\c -> c `elem` guessedChars g) $ toLower <$> toGuess g

isLost :: Game -> Bool
isLost g = fails g >= 7

guess :: Game -> Char -> Game
guess (Game fails toGuess guessed) c = Game (if c `elem` toGuess then fails else fails + 1) toGuess (c:guessed)

render :: Game -> String
render (Game fails toGuess guessed) = (hangmanRender !! fails) ++ "\n" ++ shadowUnguessed toGuess guessed ++ "\nAlready Guessed: " ++ guessed

shadowUnguessed :: String -> String -> String
shadowUnguessed toGuess guessed = (\ c -> if c `elem` guessed then c else '_') . toLower <$> toGuess

gameLoop :: Game -> IO()
gameLoop g
  | isLost g =  putStrLn "You Lost"
  | isWon g = putStrLn "You Won"
  | otherwise = do
  c <- inputChar $ guessedChars g
  let game =  guess g c
  putStrLn $ render game
  hFlush stdout
  gameLoop game

inputChar :: String -> IO Char
inputChar guessed = do
  putStrLn "Guess Letter"
  hFlush stdout
  c  <- toLower <$> getChar
  case checkChar  c guessed of Just s -> do putStrLn s
                                            inputChar guessed 
                               Nothing -> return c

checkChar :: Char -> String -> Maybe String
checkChar c g
  | c `elem` g = Just $ "You already guessed " ++ [c]
  | c == '\n' = Just "You Entered Enter..."
  | otherwise = Nothing


loadRandomWord :: IO String
loadRandomWord = do
                   g <- getStdGen
                   let r = head (randomRs (0, length wordsToGuess) g :: [Int])
                   return $ wordsToGuess !! r


wordsToGuess :: [String]
wordsToGuess = ["Happy", "GuessMe", "Yay", "Tannenbaum", "Haskell"]

hangmanRender :: [String]
hangmanRender = ["\n\
\                       \n\
\                       \n\
\                       \n\
\                       \n\
\                       \n\
\                       \n\
\                =========","\n\
\                  +---+\n\
\                  |   |\n\
\                      |\n\
\                      |\n\
\                      |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                      |\n\
\                      |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                  |   |\n\
\                      |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                 /|   |\n\
\                      |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                 /|\\  |\n\
\                      |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                 /|\\  |\n\
\                 /    |\n\
\                      |\n\
\                =========", "\n\
\                  +---+\n\
\                  |   |\n\
\                  O   |\n\
\                 /|\\  |\n\
\                 / \\  |\n\
\                      |\n\
\                ========="]
