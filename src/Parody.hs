module Parody(parody) where

import Markov(markov)
import System.Random(RandomGen)

parody :: (RandomGen r) => [String] -> r -> Int -> (String, r)
parody input rng n = (unwords output, rng')
                     where (output, rng') = markov (map words input) rng n
