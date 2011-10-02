-- solver for 9x9 sudoku

import Data.List

type Cell = Int
type GameState = [Cell]
type CellPos = Int -- position is just an index of the flat gamestate table
type Rownum = Int
type Colnum = Int

easy1 :: GameState
easy1 = [7,8,0, 5,0,2, 0,3,0,
         9,6,5, 3,0,8, 7,4,2,
         3,2,1, 7,0,4, 9,0,5,
     
         8,5,9, 0,4,7, 0,1,0,
         6,0,7, 2,3,0, 8,5,0,
         2,0,3, 0,8,5, 4,0,6,
     
         5,0,8, 4,2,3, 0,6,7,
         4,0,6, 1,5,9, 0,2,8,
         1,3,2, 0,7,6, 5,9,0]

hard1 :: GameState
hard1 = [0,0,1, 0,2,0, 0,0,0,
         0,8,0, 0,0,6, 0,2,0,
         0,3,0, 9,0,0, 8,0,0,
         
         8,0,0, 7,0,0, 0,0,0,
         6,0,7, 0,0,0, 5,0,0,
         3,0,4, 0,8,0, 9,0,0,
         
         0,0,0, 0,0,0, 0,0,2,
         0,0,0, 0,0,5, 0,0,8,
         0,0,0, 0,3,0, 7,0,0]
         
sudokusize = 9;

showGs :: GameState -> IO()
showGs gs = putStrLn $ showRows [0,1,2] ++ "\n" ++ 
                       showRows [3,4,5] ++ "\n" ++ 
                       showRows [6,7,8]
    where showRows = foldl (\str rownum -> str ++ showRow gs rownum ++ "\n") ""          
               
showRow :: GameState -> Rownum -> String
showRow gs rownum = foldl step "" $ zip [0..] $ cellsOfRow rownum gs
    where step str (pos, value) | pos == 0 = show value
                                | pos `mod` 3 == 0 = str ++ "   " ++ show value
                                | otherwise = str ++ " " ++ show value
                                
rowOfCell :: CellPos -> Rownum
rowOfCell pos = pos `div` sudokusize

colOfCell :: CellPos -> Colnum
colOfCell pos = pos - sudokusize * rowOfCell pos

cellsOfRow :: Rownum -> GameState -> [Cell]
cellsOfRow rownum = take sudokusize . drop (rownum*sudokusize)

cellsOfCol :: Colnum -> GameState -> [Cell]
cellsOfCol colnum = snd . unzip . filter (colmatch) . zip [0..]
    where colmatch (idx,cell) = colnum == (colOfCell idx)

-- upperleft corner of 3x3 area in 9x9 table
cornerOf3x3Area :: CellPos -> (Rownum, Colnum)
cornerOf3x3Area cellpos = (rowOfCell cellpos `div` 3 * 3, colOfCell cellpos `div` 3 * 3)

take3 :: GameState -> (Rownum, Colnum) -> [Cell]
take3 gs (row, col) = take 3 $ drop (col + row*sudokusize) gs

cellsOf3x3Area :: CellPos -> GameState -> [Cell]
cellsOf3x3Area cellpos gs = (take3 gs corner) ++ 
                            (take3 gs $ addRow 1 corner) ++ 
                            (take3 gs $ addRow 2 corner)
    where corner = cornerOf3x3Area cellpos
          addRow n (row, col) = (row+n, col)          
          
getValidCells :: GameState -> CellPos -> [Cell]
getValidCells gs pos | gs !! pos > 0 = []
                     | otherwise = [1..9] \\ (row ++ column ++ area)
    where row = cellsOfRow (rowOfCell pos) gs
          column = cellsOfCol (colOfCell pos) gs
          area = cellsOf3x3Area pos gs
          
getValidCellCount :: GameState -> CellPos -> Int        
getValidCellCount gs pos = length $ getValidCells gs pos

isEndState :: GameState -> Bool
isEndState gs = all ( 0< ) gs

getFreeCells :: GameState -> [(CellPos, Cell)]
getFreeCells =  filter (\(pos, val) -> val == 0) . zip [0..]

isDeadEnd :: GameState -> Bool
isDeadEnd gs = test $ getFreeCells gs
    where test [] = True
          test ((pos, val) : rest) | 0 < getValidCellCount gs pos = False
                                   | otherwise = test rest
                    
getFirstFreeCellWithLeastOptions :: GameState -> CellPos
getFirstFreeCellWithLeastOptions gs = fst $ snd $ head
                                        $ sort $ filter (\(count, (pos, val)) -> count > 0)                                          
                                          $ map (\(pos, val) -> (getValidCellCount gs pos, (pos, val))) 
                                            $ getFreeCells gs

place :: GameState -> CellPos -> Cell -> GameState                                              
place gs cellpos newval = snd $ unzip $ map setVal $ zip [0..] gs
    where setVal (pos, val) | pos == cellpos = (pos, newval)
                            | otherwise = (pos, val)    
                            
play :: GameState -> GameState
play gs | isEndState gs = gs
play gs | isDeadEnd gs = gs
play gs = do
    let cellpos = getFirstFreeCellWithLeastOptions gs
    let options = getValidCells gs cellpos
    let newstates = map (place gs cellpos) options    
    -- recurse until we find an end:
    let endstates = map (play) newstates     
    -- the endstates are either deadend or the actual solution states
    if [] == filter isEndState endstates then head endstates
        else head $ filter isEndState endstates
    
    
-- test it    
test1 = showGs $ play easy1
test2 = showGs $ play hard1