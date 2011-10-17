-- solver for 9x9 sudoku

import Data.List

type Value = Int
type GameState = [Value]
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
showRow gs rownum = foldl step "" $ zip [0..] $ valsOfRow rownum gs
    where step str (pos, value) | pos == 0 = show value
                                | pos `mod` 3 == 0 = str ++ "   " ++ show value
                                | otherwise = str ++ " " ++ show value
                                
rowOfCell :: CellPos -> Rownum
rowOfCell pos = pos `div` sudokusize

colOfCell :: CellPos -> Colnum
colOfCell pos = pos - sudokusize * rowOfCell pos

valsOfRow :: Rownum -> GameState -> [Value]
valsOfRow rownum = take sudokusize . drop (rownum*sudokusize)

valsOfCol :: Colnum -> GameState -> [Value]
valsOfCol colnum = snd . unzip . filter (colmatch) . zip [0..]
    where colmatch (idx,cell) = colnum == (colOfCell idx)

-- upperleft corner of 3x3 area in 9x9 table
cornerOf3x3Area :: CellPos -> (Rownum, Colnum)
cornerOf3x3Area cellpos = (rowOfCell cellpos `div` 3 * 3, colOfCell cellpos `div` 3 * 3)

take3 :: GameState -> (Rownum, Colnum) -> [Value]
take3 gs (row, col) = take 3 $ drop (col + row*sudokusize) gs

cellsOf3x3Area :: CellPos -> GameState -> [Value]
cellsOf3x3Area cellpos gs = (take3 gs corner) ++ 
                            (take3 gs $ addRow 1 corner) ++ 
                            (take3 gs $ addRow 2 corner)
    where corner = cornerOf3x3Area cellpos
          addRow n (row, col) = (row+n, col)          
          
getValidCells :: GameState -> CellPos -> [Value]
getValidCells gs pos | gs !! pos > 0 = []
                     | otherwise = [1..9] \\ (row ++ column ++ area)
    where row = valsOfRow (rowOfCell pos) gs
          column = valsOfCol (colOfCell pos) gs
          area = cellsOf3x3Area pos gs
          
getValidCellCount :: GameState -> CellPos -> Int        
getValidCellCount gs pos = length $ getValidCells gs pos

isSolutionState :: GameState -> Bool
isSolutionState gs = all ( 0< ) gs

getPositionsOfFreeCells :: GameState -> [CellPos]
getPositionsOfFreeCells =  map (fst) . filter (\(pos, val) -> val == 0) . zip [0..]

isDeadEnd :: GameState -> Bool
isDeadEnd gs = test $ getPositionsOfFreeCells gs
    where test [] = True
          test (position : rest) | 0 < getValidCellCount gs position = False
                                 | otherwise = test rest
                    
getFirstFreeCellWithLeastMoves :: GameState -> (Int, CellPos, [Value])
getFirstFreeCellWithLeastMoves gs = head
                                      $ sort 
                                        -- add valuecount, to sort to get the one with least options
                                        $ map (\(pos, vals) -> (length vals, pos, vals)) 
                                          $ getMoves gs

getMoves :: GameState -> [(CellPos, [Value])]
getMoves gs = map (\pos -> (pos, getValidCells gs pos)) 
                  $ getPositionsOfFreeCells gs

place :: GameState -> CellPos -> Value -> GameState                                              
place gs cellpos newval = snd $ unzip $ map setVal $ zip [0..] gs
    where setVal (pos, val) | pos == cellpos = (pos, newval)
                            | otherwise = (pos, val)    
                            

-- best first search, which does not stop when solution is found 
play :: GameState -> GameState
play gs | isSolutionState gs = gs
play gs | isDeadEnd gs = gs
play gs | 0 == length (getPositionsOfFreeCells gs) = gs
play gs = do
    let (movecount, cellpos, moves) = getFirstFreeCellWithLeastMoves gs
    let newstates = map (place gs cellpos) moves    
    -- recurse with map means we will go through each recursion tree even if solution is found
    let endstates = map (play) newstates
    let solutionstates = filter isSolutionState endstates     
    -- will return the deadend state ff solutionstate is not found
    if [] == solutionstates then easy1 else head solutionstates

-- best first search, which stops searching when solution is found 
playWithEagerStop :: [GameState] -> [GameState]
playWithEagerStop [] = [] 
playWithEagerStop (gs:rest) | isSolutionState gs = [gs]
                | isDeadEnd gs = []
                | otherwise = do
    let (movecount, cellpos, moves) = getFirstFreeCellWithLeastMoves gs
    let newstates = map (place gs cellpos) moves    
    -- recurse until we find an end:
    let solutionstates = playWithEagerStop newstates
    if [] == solutionstates then playWithEagerStop rest else solutionstates


-- does not work with hard1, because it leads to too many different states
playBreadthFirst :: Int -> [GameState] -> [GameState]
playBreadthFirst limit gstates | limit == 0 = gstates
--playBreadthFirst _ [] = []
playBreadthFirst _ gstates | any (isSolutionState) gstates = filter (isSolutionState) gstates
playBreadthFirst _ gstates | all (isDeadEnd) gstates = []
playBreadthFirst limit gstates = playBreadthFirst (limit - 1)  
                             $ nub 
                               $ concat
                                 $ map moveToNewStates
                                   $ filterOutDeadEnds gstates
    where filterOutDeadEnds gstates = filter (\state -> not (isDeadEnd state))  gstates                                    

moveToNewStates :: GameState -> [GameState]
moveToNewStates gs = do
    let (movecount, cellpos, values) = getFirstFreeCellWithLeastMoves gs
    placeMoves gs cellpos values

placeMoves :: GameState -> CellPos -> [Value] -> [GameState]    
placeMoves gs pos values = map (place gs pos) values 
              
          

-- TESTING

-- test depth first which does not stop when solution is found    
testDepthFirstWithEasy = showGs $ play easy1
testDepthFirstWithHard = showGs $ play hard1

-- test breadth first    
testBreadthFirstEasy = showGs $ head $ playBreadthFirst 25 [easy1]
testBreadthFirstHard = showGs $ head $ playBreadthFirst 10 [hard1]

-- test best first which stops when solution is found
testEagerStopWithEasy = showGs $ head $ playWithEagerStop [easy1]
testEagerStopWithHard = showGs $ head $ playWithEagerStop [hard1]
