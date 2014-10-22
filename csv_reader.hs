import System.Environment 

main = do
    args <- getArgs
    sumRowsInCsv (head args)

sumRowsInCsv :: String -> IO ()  
sumRowsInCsv fileName = do
    contents <- readFile fileName  
    let sums = map parseLine (lines contents)  
    putStrLn $ show sums 
    where parseLine :: String -> Integer
          parseLine = sumStrings . splitOn ';'
          sumStrings = sum . map read

-- tämä löytyisi kyllä valmiina, mutta toteutetaan huvikseen itse
splitOn :: Char -> String -> [String]
splitOn sep str = splitOnRecur sep [[]] str
    where splitOnRecur :: Char -> [String] -> String -> [String]
          splitOnRecur sep result [] = result
          splitOnRecur sep result (x:rest) | sep == x  = splitOnRecur sep ([]:result) rest
                                           | otherwise = splitOnRecur sep (appendChar x result) rest
          appendChar :: Char -> [String] -> [String]
          appendChar ch (str : rest) = (ch : str) : rest

