module Markov(markov) where

import Data.Map(Map)
import qualified Data.Map as Map(alter, empty, lookup)
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq(index, length, singleton)
import System.Random(RandomGen, randomR)

type ChainRules a = Map [a] (Seq a)

-- | Take a list of values (the input data) and generate Markov mappings of order n, where n is the number
--   previous values to consider when determining the next value.  The map key is a list (of length <= n) of values
--   and the entry is a sequence of valid next values (may contain duplicates for values that are more likely to
--   occur than others).
deriveRules :: Ord a => [a] -> Int -> ChainRules a
deriveRules input n = buildRules input n $ initRules $ take n input

-- | As well has having rules for each sequence of n values, we need a few other rules for shorter sequences
--   (with lengths from 0 to n-1) at the start so that we can start building the output from an empty list.
initRules :: Ord a => [a] -> ChainRules a
initRules [] = Map.empty
initRules input = updateRule start (last input) $ initRules start
                  where start = init input

-- | This builds the other rules, those that are derived from a full sequence of n previous values.
buildRules :: Ord a => [a] -> Int -> ChainRules a ->  ChainRules a
buildRules input n rules
    | length input > n = buildRules (tail input) n $ updateRule (take n input) (input !! n) rules
    | otherwise        = rules

-- | Creates a new rule for a given context or updates the existing rule by adding to the sequence of valid
--   alternatives for that context.
updateRule :: Ord a => [a] -> a -> ChainRules a -> ChainRules a
updateRule context value = Map.alter (addToSequence value) context

-- | Add a value to the sequence of valid alternatives for a given context.  Creates the sequence if it doesn't exist.
addToSequence :: Ord a => a -> Maybe (Seq a) -> Maybe (Seq a)
addToSequence value Nothing       = Just $ Seq.singleton value
addToSequence value (Just values) = Just $ values |> value


-- | The next value is determined by the most recent n values (the context) and is randomly selected from the sequence
--   of valid alternatives in the Markov mappings.
buildOutput :: (RandomGen r, Ord a) => [a] -> ChainRules a -> r -> Int -> ([a], r)
buildOutput output rules rng n = case nextValue output rules rng n of
                                     (Nothing, rng') -> (output, rng')
                                     (Just value, rng') -> buildOutput (output ++ [value]) rules rng' n

nextValue :: (RandomGen r, Ord a) => [a] -> ChainRules a -> r -> Int -> (Maybe a, r)
nextValue output rules rng n = case Map.lookup context rules of
                                   Nothing  -> (Nothing, rng)
                                   Just seq -> (Just $ Seq.index seq i, rng')
                                               where (i, rng') = randomR (0, Seq.length seq - 1) rng
                               where context = drop (length output - n) output

-- | Build a Markov Chain of order n from the input sequence.
markov :: (RandomGen r, Ord a) => [a] -> r -> Int -> [a]
markov input rng n = fst $ buildOutput [] rules rng n
                     where rules = deriveRules input n
