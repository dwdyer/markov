import Data.Map(Map)
import qualified Data.Map as Map(alter, empty, lookup)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(index, length, singleton)
import System.Environment(getArgs)
import System.Random(RandomGen, getStdGen, randomR)

type WordMap = Map [String] (Seq String)

-- | Take a list of words (the source text) and generate Markov mappings of order n, where n is the number
--   previous words to consider when determining the next word.  The map key is a list of words (of length <= n)
--   and the value is a sequence of valid next words (may contain duplicates for words that are more likely to
--   occur than others).
deriveRules :: [String] -> Int -> WordMap
deriveRules words n = buildRules words n $ initRules $ take n words

-- | As well has having rules for each sequence of n words, we need a few other rules for shorter sequences
--   (with lengths from 0 to n-1) at the start of the text so that we can start building the output from an empty
--   list of words.
initRules :: [String] -> WordMap
initRules [] = Map.empty
initRules words = appendRule start (last words) $ initRules start
                  where start = take (length words - 1) words

-- | This builds the other rules, those that are derived from a full sequence of n previous words.
buildRules :: [String] -> Int -> WordMap ->  WordMap
buildRules words n map
    | length words > n = buildRules (tail words) n $ appendRule (take n words) (words !! n) map
    | otherwise        = map

appendRule :: [String] -> String -> WordMap -> WordMap
appendRule context word map = Map.alter (appendWord word) context map

appendWord :: String -> Maybe (Seq String) -> Maybe (Seq String)
appendWord word Nothing      = Just $ Seq.singleton word
appendWord word (Just words) = Just $ words |> word


-- | The next word is determined by the most recent n words and is randomly selected from the sequence of valid
--   alternatives in the Markov mappings.
addNextWord :: (RandomGen r) => [String] -> WordMap -> r -> Int -> ([String], r)
addNextWord words map rng n = case nextWord words map rng n of
                                  (Nothing, rng') -> (words, rng')
                                  (Just w, rng') -> addNextWord (words ++ [w]) map rng' n

nextWord :: (RandomGen r) => [String] -> WordMap -> r -> Int -> (Maybe String, r)
nextWord words map rng n = case Map.lookup key map of
                               Nothing  -> (Nothing, rng)
                               Just seq -> (Just $ Seq.index seq i, rng')
                                           where (i, rng') = randomR (0, Seq.length seq - 1) rng
                           where key = drop (length words - n) words


parody :: (RandomGen r) => String -> r -> Int -> String
parody input rng n = unwords . fst $ addNextWord [] rules rng n
                     where rules = deriveRules (words input) n


-- | The main function expects two arguments, the path of an input text file and the number of previous words to
--   consider when constructing the chain.
main :: IO ()
main = do (inputPath:n:_) <- getArgs
          input <- readFile inputPath
          rng <- getStdGen
          print $ parody input rng $ read n
