module Main where

import System.Random
import Control.Monad.Random.Class
import Uno

forever :: (Monad m) => m a -> m b
forever a = a >> forever a

mainStartingTurnCounts :: IO ()
mainStartingTurnCounts = do
  let loop = do
        newStdGen
        rng <- getStdGen
        game <- dealM baseDeck 5
        options <- plays game
        (print . length) options
  forever loop

main :: IO ()
main = do
  game <- dealM baseDeck 5
  turnCount <- playOut game
  print turnCount
