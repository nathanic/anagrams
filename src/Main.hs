import Data.AnaTree

import           Control.Monad (forM_)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           System.Random (randomRIO)
import qualified System.Console.ArgParser as AP

main :: IO ()
main = AP.runApp (AP.mkDefaultApp argSpec "anagrams") makeAnagrams

makeAnagrams :: Args -> IO ()
makeAnagrams (Args rando wordfile term limit) = do
    lexicon <- pruneLexicon . Text.lines <$> Text.readFile wordfile
    let tree    = buildTree lexicon
        snip    = if limit > 0 then take limit else id
        results = snip . findFullAnagrams tree $ prepareTerm $ Text.pack term
    if rando
      then do
        n <- randomRIO (0, 10000)
        putStrLn $ unwords $ map Text.unpack (results !! n)
      else
        forM_ results $ putStrLn . unwords . map Text.unpack


data Args = Args { argRandom :: Bool
                 , argWordFile :: String
                 , argTerm :: String
                 , argCount :: Int
                 }
  deriving (Show)

argSpec :: AP.ParserSpec Args
argSpec =
  Args
    `AP.parsedBy` AP.boolFlag "random" `AP.Descr` "select anagram results randomly"
    `AP.andBy` AP.optFlag "words" "wordfile" `AP.Descr` "name of wordlist file"
    `AP.andBy` AP.reqPos "term" `AP.Descr` "the term for which to find anagrams"
    `AP.andBy` AP.optPos (-1) "count" `AP.Descr` "limit output to `count` results"

