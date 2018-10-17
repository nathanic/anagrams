-- http://blog.notdot.net/2007/10/Damn-Cool-Algorithms-Part-3-Anagram-Trees
module Data.AnaTree where

import           Control.Monad (guard)
import           Data.Char    (isAlpha, isNumber, isUpper, toLower)
import           Data.List    (foldl', group, head, length, sort, delete)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
--import qualified Data.Text.IO as T

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

data AnaTree =
      AnaBranch { atChildren :: Map Int AnaTree }
    | AnaLeaf   { atTerms :: Set Text }
  deriving (Eq, Show)

emptyAnaTree :: AnaTree
emptyAnaTree = AnaBranch mempty

-- alphabet ranked by descending frequency in English usage
-- https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
alphabet :: [Char]
alphabet = "etaoinsrhdlucmfywgpbvkxqjz"
-- alphabet = "cba" -- reduced for testing

  -- TODO: make ü count as u, etc.
charFrequency :: Char -> Text -> Int
charFrequency needle = T.foldl' (\acc c -> if toLower c == needle then acc + 1 else acc) 0

newtype Histogram = Histogram (Map Char Int)
  deriving (Eq, Show)

-- not used yet
makeHistogram :: Text -> Histogram
makeHistogram term = Histogram $ Map.fromList [(head chargroup, length chargroup)
                                                | chargroup <- group (sort $ T.unpack term)]

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

-- build an AnaTree from a list of terms
buildTree :: [Text] -> AnaTree
buildTree = foldl' (flip insertTerm) emptyAnaTree

-- | make an updated AnaTree with the specified term added to it
insertTerm :: Text -> AnaTree -> AnaTree
insertTerm term = computeLayer alphabet
  where
    -- build layers of the tree by recursing down the alphabet
    computeLayer :: [Char] -> AnaTree -> AnaTree
    computeLayer []            (AnaLeaf terms)  = AnaLeaf $ Set.insert term terms
    computeLayer (c:alpharest) (AnaBranch kids) =
        let freq = charFrequency c term
            upsert :: Maybe AnaTree -> Maybe AnaTree
            upsert existing = let defaultSubtree = if null alpharest
                                                    then AnaLeaf mempty
                                                    else AnaBranch mempty
                                  subtree = fromMaybe defaultSubtree existing
                               in Just $ computeLayer alpharest subtree
         in AnaBranch $ Map.alter upsert freq kids

-- | search the tree for all paths that involve less than or equal frequency counts to our term
-- | and return all terms found at the winning leaves
findSubAnagrams :: AnaTree -> Text -> [Text]
findSubAnagrams tree term = go tree alphabet
  where
    go (AnaLeaf terms)  _             = Set.toList terms
    go (AnaBranch kids) (c:alpharest) = do
        (freq, subtree) <- Map.toAscList kids
        guard $ freq <= charFrequency c term -- TODO: use histogram instead of counting every time?
        go subtree alpharest

-- | lazily search for multi-word groups that are exact anagrams of the given term
findFullAnagrams :: AnaTree -> Text -> [[Text]]
findFullAnagrams tree term = prune $ do
    sub <- findSubAnagrams tree term
    let residual = subtractTerm term sub
    if residual == mempty
        then return [sub]
        else do
            remaining <- findFullAnagrams tree residual
            return $ sub : remaining
  where
--     prune = nub . map sort
    prune = id

-- produce an anagram by making bounded-random choices when looking for subanagrams
-- feels dirty
subtractTerm :: Text -> Text -> Text
subtractTerm t1 t2 = T.pack $ foldl' (flip delete) (T.unpack t1) (T.unpack t2)


