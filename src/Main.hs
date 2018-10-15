import AnaTree

import           Control.Monad (forM_)
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           System.Environment (getArgs)

main = do
    lexicon <- pruneLexicon . Text.lines <$> (Text.readFile "words")
    putStrLn $ "loaded lexicon: " ++ (show (length lexicon)) ++ " terms"
    let tree = buildTree lexicon
    args <- getArgs
    let (term, limit) = case args of
                            (t:l:_) -> (t, read l :: Int)
                            [t]     -> (t, maxBound)
                            []      -> ("fartknocker", maxBound)
    putStrLn $ "searching for anagrams for '" ++ term ++ "'"
    let results = take limit . findFullAnagrams tree $ prepareTerm $ Text.pack term
    forM_ results $ \res ->
        putStrLn $ show res
    putStrLn $ "Found " ++ (show $ length results) ++ " anagrams."

