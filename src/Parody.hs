module Parody(parody) where

import Control.Monad(replicateM)
import Control.Monad.State(State)
import Markov(markov)
import System.Random(RandomGen)

parody :: (RandomGen r) => [String] -> Int -> Int -> State r String
parody input n m = do output <- replicateM m $ markov (map words input) n
                      return . unlines $ map unwords output
