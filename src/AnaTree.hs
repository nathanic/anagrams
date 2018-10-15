-- http://blog.notdot.net/2007/10/Damn-Cool-Algorithms-Part-3-Anagram-Trees
module AnaTree where

import           Control.Monad (guard)
import           Data.Char    (isAlpha, isNumber, isUpper, toLower)
import           Data.List    (foldl', foldl1', group, head, length, sort, delete, nub)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

prepareTerm :: Text -> Text
prepareTerm = T.toLower . T.filter isAlpha

pruneLexicon :: [Text] -> [Text]
pruneLexicon = filter appropes
  where
    appropes term = not $ any ($ term)
                              [ T.null
                              , isAcronym
                              , isSlashy
                              , isDotted
                              , isHyphened
                              , isProper
                              , isNumeric
                              , hasNoVowels
                              ]
    isAcronym = T.all isUpper
    isNumeric = T.any isNumber
    isSlashy = T.any (== '/')
    isDotted = T.any (== '.')
    isHyphened term = T.pack "-" `T.isPrefixOf` term ||
                      T.pack "-" `T.isSuffixOf` term
    isProper = T.any isUpper
    hasNoVowels = not . T.any (`elem` "aeoiuy")

type AnagramResults = [[Text]]

data AnaTree =
      AnaBranch { atChildren :: Map Int AnaTree }
    | AnaLeaf   { atTerms :: Set Text }
  deriving (Eq, Show)

newtype Histogram = Histogram (Map Char Int)
  deriving (Eq, Show)

makeHistogram :: Text -> Histogram
makeHistogram term = Histogram $ Map.fromList [(head chargroup, length chargroup)
                                                | chargroup <- group (sort $ T.unpack term)]

emptyAnaTree :: AnaTree
emptyAnaTree = AnaBranch Map.empty

-- alphabet ranked by descending frequency in English usage
-- https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
alphabet :: [Char]
alphabet = "etaoinsrhdlucmfywgpbvkxqjz"
-- alphabet = "cba" -- reduced for testing

charFrequency :: Char -> Text -> Int
charFrequency needle = T.foldl' (\acc c -> if toLower c == needle then acc + 1 else acc) 0

-- | Represents a word from the lexicon enriched with state for the tree-building process
data EnrichedTerm = EnrichedTerm { etWord :: Text         -- verbatim text of word
                                 , etHist :: Map Char Int -- "unconsumed" histogram
                                 }
  deriving Show

-- | Annotate a term (word) with a character histogram.
-- enrichTerm "abacab" == EnrichedTerm "abacab" (Map.fromList [('a': 3), ('b': 2), ('c': 1)])
enrichTerm :: Text -> EnrichedTerm
enrichTerm term = EnrichedTerm term hist
  where
    hist = Map.fromList [(head chargroup, length chargroup)
                         | chargroup <- group (sort $ T.unpack term)]


getCharFrequency :: Char -> EnrichedTerm -> Int
getCharFrequency c term = fromMaybe 0 $ Map.lookup c (etHist term)

-- | Derive a new EnrichedTerm by subtracting one instance of a particular character from its histogram.
-- Used to 'consume' characters during tree-building.
-- Will throw a runtime error if the character is not found in the term!
subtractChar :: Char -> EnrichedTerm -> EnrichedTerm
subtractChar c term = EnrichedTerm (etWord term) (Map.alter f c (etHist term))
  where
    f Nothing      = error $ "Attempted to subtract character " ++ [c] ++
                               " that is not present in histogram " ++ (show $ etHist term)
    f (Just count) = Just (count - 1)

{-
Assume we have the following information:

    A lexicon or dictionary of words to populate the tree with
    An alphabet for words in the lexicon
    The tree we are building
    A current node

For each term in the lexicon:

    Generate a letter-frequency histogram for the term.
    Set the current node to the root of the tree.
    For each symbol in the alphabet:
        Get the frequency of the current symbol in the current term. Call it f
        Set the current node to the fth child node of the current node, creating it if it doesn't exist
    Append the current term to the list of words on the current (leaf) node
-}

buildTree :: [Text] -> AnaTree
buildTree = foldl' (flip insertTerm) emptyAnaTree

insertTerm :: Text -> AnaTree -> AnaTree
insertTerm term = computeLayer alphabet
  where
    -- build layers of the tree by recursing down the alphabet
    computeLayer :: [Char] -> AnaTree -> AnaTree
    computeLayer []            (AnaLeaf terms)  = AnaLeaf $ Set.insert term terms
    computeLayer (c:alpharest) (AnaBranch kids) =
        let freq = charFrequency c term
            upsert :: Maybe AnaTree -> Maybe AnaTree
            upsert existing = let defaultSubtree = if alpharest == []
                                                    then AnaLeaf mempty
                                                    else AnaBranch mempty
                                  subtree = fromMaybe defaultSubtree existing
                               in Just $ computeLayer alpharest subtree
         in AnaBranch $ Map.alter upsert freq kids

-- search the tree for all paths that involve less than or equal frequency counts to our term
findSubAnagrams :: AnaTree -> Text -> [Text]
findSubAnagrams tree term = go tree alphabet
  where
    go (AnaLeaf terms)  _             = Set.toList terms
    go (AnaBranch kids) (c:alpharest) = do
        (freq, subtree) <- Map.toAscList kids
        guard $ freq <= charFrequency c term
        go subtree alpharest

-- for each sub anagram
--   compute residual
--   find subanagrams of residual
--   if no subanagrams for residual
--     drop this sub anagram

findFullSets :: AnaTree -> Text -> [[Text]]
findFullSets tree term = prune $ do
    sub <- findSubAnagrams tree term
    let residual = subtractTerm term sub
    if residual == mempty
        then return [sub]
        else do
            remaining <- findFullSets tree residual
            return $ sub : remaining
  where
--     prune = nub . map sort
    prune = id

subtractTerm t1 t2 = T.pack $ foldl' (flip delete) (T.unpack t1) (T.unpack t2)


testWords :: [Text]
testWords = map T.pack ["a", "ab", "ba", "cab", "abacab", "acabab"]

testTree = buildTree testWords
{-
testTree =
    AnaBranch {atChildren = fromList [(0,AnaBranch {atChildren = fromList [(0,AnaBranch {atChildren = fromList [(1,AnaLeaf {atTerms = fromList ["a"]})]}),
                                                                           (1,AnaBranch {atChildren = fromList [(1,AnaLeaf {atTerms = fromList ["ab","ba"]})]})]}),
                                      (1,AnaBranch {atChildren = fromList [(1,AnaBranch {atChildren = fromList [(1,AnaLeaf {atTerms = fromList ["cab"]})]}),
                                      (2,AnaBranch {atChildren = fromList [(3,AnaLeaf {atTerms = fromList ["abacab","acabab"]})]})]})]}
-}
