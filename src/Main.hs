module Main where

import Control.Monad.State(runState)
import Parody(parody)
import System.Environment(getArgs)
import System.Random(getStdGen, setStdGen)

-- | The main function expects three arguments, the path of an input text file, the number of previous words to
--   consider when constructing the chain, and the number of outputs to generate.  Each line of the text file is
--   treated as a separate piece of text.
main :: IO ()
main = do (inputPath:n:m:_) <- getArgs
          input <- readFile inputPath
          rng <- getStdGen
          let (output, rng') = runState (parody (lines input) (read n) (read m)) rng
          setStdGen rng'
          putStr output
