import qualified Data.Map as Map
import Data.List

data IndexItem = IndexItem { file::String, line::Int, linePos::Int, word::String } deriving Show
type Index = Map.Map String [IndexItem]

emptyIndex = Map.empty

insertIdxItem :: Index -> IndexItem -> Index
insertIdxItem index item = Map.insertWith (++) (word item) [item] index

handleLines :: FilePath -> String -> Index
handleLines filepath content = foldl step emptyIndex $ zipWordsWithIdx $ zipLinesWithIdx content
    where zipLinesWithIdx :: String -> [(Int, String)]
          zipLinesWithIdx = zipWithIndex . lines
          zipWordsWithIdx :: [(Int, String)] -> [(Int, Int, String)]
          zipWordsWithIdx numberedLines = flatten $ map (handleLine) numberedLines          
          handleLine :: (Int, String) -> [(Int, Int, String)]
          handleLine (linePos, line) = appendCol linePos (zipWithIndex $ words line)
          step :: Index -> (Int, Int, String) -> Index
          step idx (linePos, wordPos, word) = insertIdxItem idx (IndexItem filepath linePos wordPos word)
      
appendCol :: a -> [(b, c)] -> [(a, b, c)]
appendCol colToAppend = map (\pair -> (colToAppend, fst pair, snd pair))
      
handle = zipWithIndex . lines
zipWithIndex = zip [1..]

flatten :: [[a]] -> [a]
flatten = foldl (++) []
