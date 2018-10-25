import Data.AnaTree

import           Data.List (nub,sort)
import           Control.Monad (forM_)
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           System.Random (randomRIO)
import qualified System.Console.ArgParser as AP

import           Data.Random (RVar, sampleFrom)
import           Data.Random.List (shuffle)
import qualified System.Random.MWC as MWC

main :: IO ()
main = AP.runApp (AP.mkDefaultApp argSpec "anagrams") makeAnagrams

makeAnagrams :: Args -> IO ()
makeAnagrams (Args rando wordfile poolSize term limit) = do
    gen <- MWC.createSystemRandom
    lexicon <- loadLexicon gen
    let tree    = buildTree lexicon
        uniqify = nub . map sort -- ignore equivalent orderings
        snip    = if limit > 0 then take limit else id
        term'   = prepareTerm $ Text.pack term
        -- gaze not too deeply into `alles`; can literally take hours to compute
        alles   = findFullAnagrams tree term'
    results <- if rando
      then
        sampleFrom gen $ do
          pool <- mapM shuffle $ uniqify $ take (max poolSize limit) alles
          snip <$> shuffle pool
      else
        return $ snip alles
    forM_ results $ putStrLn . unwords . map Text.unpack
  where
    loadLexicon :: MWC.GenIO -> IO [Text]
    loadLexicon gen = do
      lexicon <- pruneLexicon . Text.lines <$> Text.readFile wordfile
      if rando
        then sampleFrom gen $ shuffle lexicon
        else return lexicon

data Args = Args { argRandom :: Bool
                 , argWordFile :: String
                 , argPoolSize :: Int
                 , argTerm :: String
                 , argCount :: Int
                 }
  deriving (Show)

argSpec :: AP.ParserSpec Args
argSpec =
  Args
    `AP.parsedBy` AP.boolFlag "random" `AP.Descr` "select anagram results randomly"
    `AP.andBy` AP.optFlag "words" "wordfile" `AP.Descr` "name of wordlist file"
    `AP.andBy` AP.optFlag 10000 "poolsize" `AP.Descr` "minimum pool size to draw when generating random results"
    `AP.andBy` AP.reqPos "term" `AP.Descr` "the term for which to find anagrams"
    `AP.andBy` AP.optPos (-1) "count" `AP.Descr` "limit output to `count` results"

