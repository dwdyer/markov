module Main where

import Control.Monad.State(runState)
import Parody(parody)
import System.Environment(getArgs)
import System.Random(getStdGen, setStdGen)

-- | The main function expects two arguments, the path of an input text file and the number of previous words to
--   consider when constructing the chain.  Each line of the text file is treated as a separate piece of text.
main :: IO ()
main = do (inputPath:n:_) <- getArgs
          input <- readFile inputPath
          rng <- getStdGen
          let (output, rng') = runState (parody (lines input) $ read n) rng
          setStdGen rng'
          print output
