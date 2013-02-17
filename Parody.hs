module Parody where

import Markov(markov)
import System.Environment(getArgs)
import System.Random(RandomGen, getStdGen)

-- | The main function expects two arguments, the path of an input text file and the number of previous words to
--   consider when constructing the chain.
main :: IO ()
main = do (inputPath:n:_) <- getArgs
          input <- readFile inputPath
          rng <- getStdGen
          print $ parody input rng $ read n

parody :: (RandomGen r) => String -> r -> Int -> String
parody input rng n = unwords $ markov (words input) rng n
