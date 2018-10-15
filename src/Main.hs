import AnaTree

import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           System.Environment (getArgs)

main = do
    lexicon <- filter (not . Text.null) . map prepareTerm . Text.lines <$> (Text.readFile "/usr/share/dict/words")
    let tree = buildTree lexicon
    (term:_) <- getArgs
    print $ "searching for anagrams for '" ++ term ++ "'\n"
--     let results = findFullSets tree (Text.pack term)
    let results = findSubAnagrams tree (Text.pack term)
    print $ "found: " ++ show results

