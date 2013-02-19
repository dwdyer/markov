module Parody(parody) where

import Control.Monad.State(State)
import Markov(markov)
import System.Random(RandomGen)

parody :: (RandomGen r) => [String] -> Int -> State r String
parody input n = do output <- markov (map words input) n
                    return $ unwords output
