-- Minesweeper (TP PFONC)
import Data.Set (Set)
import System.Random
import Data.List
import qualified Data.Set as S
import System.IO

-- Type declaration
data Cell = Covered Int Bool Bool | Uncovered Int | Selected
data Grid = Grid [[Cell]]

type Matrix = [[ Int ]]

-- Utility for declared types
instance Show Cell where
    show Selected = "x "
    show (Uncovered 0) = "  "
    show (Uncovered v) = (show v) ++ " "

    -- special cases for reveal after end of the game
    show (Covered (-1) True False) = "💣"
    show (Covered (-1) False True) = "😬" -- flagged an empty cell

    show (Covered _ _ True) = "🚩"
    show (Covered _ _ _) = ". " -- covered n mine flag


instance Show Grid where
    show (Grid l) = unlines $ map (concatMap show) l

-- return a grid with all cells revealed
reveal :: Grid -> Grid
reveal (Grid g) =
    let f (Covered _ True False) = (Covered (-1) True False) -- bomb not flagged
        f (Covered _ False True) = (Covered (-1) False True) -- flagged an empty cell
        f (Covered n _ False) = Uncovered n
        f c = c in
    
    Grid $ map (map f) g

-- `randSet n nrow ncol g1 g2` generates a list of `n` coordinates pairs 
-- uniformly placed on the grid with `ncol` columns and `nrow` rows
randSet :: Int -> Int -> Int -> StdGen -> StdGen -> Set (Int, Int)
randSet n nrow ncol g1 g2
    | nrow * ncol < n = error "too many mines"
    | otherwise       =
        let randL = zip (randomRs (0, nrow-1) g1) (randomRs (0, ncol-1) g2) in
        head $ dropWhile ((<n) . S.size) $ scanl' (flip S.insert) S.empty randL

-- `grid nrow ncol mcoords` generates a Grid of size (`nrow` x `ncol`) with
-- only `Covered` cells, those at indices (i, j) in `mcoords` contain mines.
grid:: Int -> Int -> Set (Int, Int) -> Grid
grid nrow ncol mcoords = Grid [
    [ Covered 0 (S.member (i, j) mcoords) False | j <- [0..(ncol-1)] ] 
                                                | i <- [0..(nrow-1)] ]

-- Indicates if a cell contains a mine
mineIndic :: Cell -> Int
mineIndic (Covered _ True _) = 1
mineIndic _ = 0

-- Returns a matrix containing ones at locations where mines are present in 
-- the Grid passed as parameter
mines :: Grid -> Matrix
mines (Grid g) = map (map mineIndic) g

-- Remove the first row, and adds a row of 0 at the end
moveUp :: Matrix -> Matrix
moveUp [] = error "unable to `moveUp` an empty list"
moveUp (x:xs) = xs ++ [replicate (length x) 0]

-- Remove the last row, and adds a row of 0 at the beginning
moveDown :: Matrix -> Matrix
moveDown [] = error "unable to `moveDown` an empty list"
moveDown (x:xs) = replicate (length x) 0 : x : init xs

-- Remove the first column, and adds a column of 0 as the last column
moveLeft :: Matrix -> Matrix
moveLeft = transpose . moveUp . transpose 

-- Remove the last column, and adds a column of 0 at the first column
moveRight :: Matrix -> Matrix
moveRight = transpose . moveDown . transpose 

-- Returns a list containing the matrix passed as parameters shifted in the
-- 8 possible directions.
gridMoves :: Matrix -> [ Matrix ]
gridMoves l = map ($ l) [
    moveUp,    moveUp.moveRight,
    moveRight, moveRight.moveDown,
    moveDown,  moveDown.moveLeft,
    moveLeft,  moveLeft.moveUp]

-- Returns the sum of two matrices
matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith $ zipWith (+)

-- Compute the mines count on each Cell of the grid as a matrix, and returns 
-- the sums of the 8 possible shifted versions of this matrix.
neighbourMap :: Grid -> Matrix
neighbourMap g = foldl1' matrixSum (gridMoves $ mines g)

-- Update a Cell with a new number of mines indicates
updateCell :: Cell -> Int -> Cell
updateCell (Covered _ hasMine hasFlag) n = Covered n hasMine hasFlag
updateCell (Uncovered _) n = Uncovered n
updateCell Selected _ = error "unable to update Selected Cell"

-- Update a grid: updates each cell with a new number of mines according to
-- the Matrix, return the updated Grid
updateGrid :: Grid -> Matrix -> Grid
updateGrid (Grid g) m = 
    Grid $ zipWith (zipWith updateCell) g m

-- apply a function to the i-th element of a list
applyi :: (a -> a) -> Int -> [a] -> [a]
applyi _ _ [] = error "cannot `applyi` on empty list"
applyi f 0 (x:xs) = (f x):xs
applyi f i (x:xs) = x : (applyi f (i-1) xs)

-- apply a function to the element at row i, and column j in a list of lists
applyij :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
applyij _ _ _ [] = error "cannot `applyij` on empty list"
applyij f 0 j (x:xss) = (applyi f j x):xss
applyij f i j (x:xss) = x : (applyij f (i-1) j xss)


-- `uncover (i, j) g` uncovers the Cell of coordinates (i, j) in grid `g`
-- TODO: check
uncover :: (Int, Int) -> Grid -> Grid
uncover (i, j) grid@(Grid g) =
    let nrow = length g
        ncol = length $ head g
        neighbourCoords = [(i', j')| i' <- [i-1..i+1], 0 <= i', i' < nrow,  
                                     j' <- [j-1..j+1], 0 <= j', j' < ncol, 
                                     (i', j') /= (i,j) ] in  

    -- If the cell does not have any mine on it, we uncover it, and recursively
    -- call uncover, else, we just return the original grid
    case (g !! i !! j) of
        Covered 0 _ False -> 
            let gNew = Grid $ applyij (const $ Uncovered 0) i j g in
            foldl' (flip uncover) gNew neighbourCoords
        
        Covered n _ False ->
            Grid $ applyij (const $ Uncovered n) i j g

        _ -> grid
        
-- returns 0 if the cell is uncovered, else 1
covIndic :: Cell -> Int
covIndic (Uncovered _) = 0
covIndic _ = 1

-- from a grid and the number of mines, indicates wheter the player won or not.
won :: Int -> Grid -> Bool
won n (Grid g) = 
    (sum $ map (sum . map covIndic) g) == n

-- toggle the flagged state of a covered Cell
toggleFlag :: Cell -> Cell
toggleFlag (Covered n hasMine hasFlag) = Covered n hasMine (not hasFlag)
toggleFlag cell = cell


-- game loop - Read-Eval-Print Loop (REPL)
loop :: Int -> Int -> Int -> Grid -> IO ()
loop i j n b@(Grid xs)
    | won n b = putStrLn "Victoire !"
    | otherwise = do
        -- affiche la grille avec la case i, j sélectionnée
        putStrLn $ show $ Grid $ applyij (const Selected) i j xs
        -- lit un caractère
        c <- getChar
        
        let maxi = (length xs) - 1
            maxj = (length $ head xs) - 1
            cell = xs !! i !! j

        case c of
            'i' -> loop (max (i - 1) 0)    j n b -- move cursor up
            'k' -> loop (min (i + 1) maxi) j n b -- move cursor down
            'j' -> loop i (max (j - 1) 0)    n b -- move cursor left
            'l' -> loop i (min (j + 1) maxj) n b -- move cursor right

            'f' -> -- toggle flag on cell (i, j)
                loop i j n (Grid $ applyij toggleFlag i j xs)
                
            -- uncover cell (i, j); BOOM ?
            -- NOTE: we can't uncover a cell with a flag on it
            'u' -> case cell of 
                Covered _ True  False -> do
                    putStrLn "Boom !"
                    putStrLn $ show $ reveal b
                Covered _ False False -> loop i j n $ uncover (i, j) b
                _ -> loop i j n b
                
            -- don't do anything
            otherwise -> loop i j n b

main :: IO ()
main = do
    -- désactive l’attente de la touche entrée pour l’acquisition
    hSetBuffering stdin NoBuffering
    -- désactive l’écho du caractère entré sur le terminal
    hSetEcho stdin False
    -- create two StdGen for random number generation
    g  <- newStdGen
    g' <- newStdGen
    -- number of mines, lines and columns
    let nmines = 10
        l = 7
        c = 10
        
    -- create the grid, add the mines, and update mines count
    let bEmpty = grid l c $ randSet nmines l c g g'
        b = updateGrid bEmpty (neighbourMap bEmpty)

    loop 0 0 nmines b -- start the REPL