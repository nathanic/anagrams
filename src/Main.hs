import Data.AnaTree

import           Data.List (nub,sort)
import           Control.Monad (forM_)
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
    lexicon <- pruneLexicon . Text.lines <$> Text.readFile wordfile
    let tree    = buildTree lexicon
        uniqify = nub . map sort
        snip    = if limit > 0 then take limit else id
        term'   = prepareTerm $ Text.pack term
    results <- if rando
      then do
        mwc <- MWC.create
        sampleFrom mwc $ do
          pool <- mapM shuffle $ uniqify $ take (max poolSize limit) $ findFullAnagrams tree term'
          snip <$> shuffle pool
            
            
      else return $ snip $ findFullAnagrams tree term'
    forM_ results $ putStrLn . unwords . map Text.unpack


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

