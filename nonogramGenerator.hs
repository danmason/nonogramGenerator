import Prelude as P
import Graphics.Image as I
import Graphics.Image.Interface
import Data.List as D
import Data.Char

type NonogramGrid = [Row] 
type Row = [Int]

--Stores what dimensions the image will be resized to, and (therefore) the size of the game grids.
gridDimensions :: Int
gridDimensions = 15

nonogramRows :: NonogramGrid -> [Row]
nonogramRows = id

nonogramCols :: NonogramGrid -> [Row]
nonogramCols = D.transpose

gridExample :: NonogramGrid
gridExample = [[1,1,1,0,1],[1,1,0,0,1],[0,1,0,0,0], [1,1,1,1,1],[0,1,1,1,0]]

{- 
  Generates a list of size numbers equal to the number of uninterrupted ones. For example, the 
  first line of gridExample [1,1,1,0,1] would return [3,1] as there are 3 ones in a row, then a 
  number of zeroes (in this case, one zero) and a one at the end
-}
lineSizes :: Row -> [Int]
lineSizes [] = []
lineSizes (0:xs) = lineSizes xs
lineSizes (1:xs) = (length num + 1) : lineSizes xss
                 where
                   (num, xss) = span (==1) xs

{- 
  Calls the fucntion above, and handles any returned as empty lists to be equal to [0]. In the case 
  of a line with no squares within the picross, such as [0,0,0,0,0], lineSizesWithZero will return 
  [0] such that it can be passed into the line printing functions and be displayed properly.
-}

lineSizesWithZero :: Row -> [Int]
lineSizesWithZero line = 
    if sizes == [] then [0] else sizes
    where
        sizes = lineSizes line

{- 
  Maps the lineSizesWithZero function accross every row and column in a nonogram grid, returning the 
  list of size number lists going vertically and the list of the size number lists going horizontally 
  in a tuple.
-}        
gridSizes :: NonogramGrid -> ([[Int]],[[Int]])
gridSizes xs = (P.map (lineSizesWithZero) rows, P.map (lineSizesWithZero) cols)
            where 
                rows = nonogramRows xs
                cols = nonogramCols xs

{- 
  The function takes in a file path and reads the image at that location as an imageY (essentially a black
  and white image type where the Y of a pixel is it's intensity/brightness). The passed-in image is resized 
  to the desired dimenions above. After this, the function thresholds the resized image, creating a binary 
  image where all pixels below a certain intensity become equal to 1 and all pixels above become 0. This is 
  assuming images have white backrgrounds + dark objects, but could be easily switched around depending on
  an images contents. 

  The binary image is then morphologically closed, a process which uses a structuring element to remove small
  holes in a binary image, such as gaps within lines. This 'closed' image is then returned from the function.

  While you can implement some basic thresholding techniques, fully in haskell, would recommend using a 
  more fully featured library (such as openCV) to generate the binary images from which to generate the puzzles.
  This library lacks many thresholding techniques, and resizing techniques, meaning the fucntionality below is
  fairly basic and doesn't always generate good binary images.
-}
generateBinaryImage :: FilePath -> IO (Image VU X Bit)
generateBinaryImage path = do
   image <- readImageY VU path
   let resizeImage = (resize Bilinear Edge (gridDimensions, gridDimensions) image) -- Resizing image
   let thresholded = (thresholdWith (PixelY (<0.8)) resizeImage) -- Binary thresholding
   let struct = fromLists [[0,1,0],[1,1,1],[0,1,0]] -- Structuring Element
   let closed = close struct thresholded -- Morphological closing
   return closed

-- Used to convert the 'Bits' from a binary image into their integer counterparts.
fromBit :: Bit -> Int
fromBit (Bit 0) = 0
fromBit (Bit 1) = 1

-- getX pixel extracts the bit value from a pixel and then fromBit is used to convert it into an integer.
pixelToInt :: Pixel X Bit -> Int
pixelToInt pixel = fromBit (getX pixel)

{- 
  Takes a path to an image, and generates the Nonogram / Picross grid from it. The path is passed into the
  generateBinaryImage function to create the binary image and the binary image is then converted to a list
  of lists of pixels (rather than the image library's underlying vector structure). The pixelToInt function
  is mapped accross these lists of lists to convert the pixels into integers and complete generation of 
  the Nonogram. 
-}
generateGrid :: FilePath -> IO NonogramGrid
generateGrid path = do
    binaryImage <- generateBinaryImage path
    let bits = (toLists binaryImage) -- Returns image as a list of lists of pixels / bits (as is a binary image)
    return (P.map (P.map (pixelToInt)) bits)

{- 
  Pads the lines at the sides of the grid such that they are all the same length, (equal to the length of the largest
  set of size numbers, passed in as 'longestLine'). The -1's act as a sort of null value, which will be replaced when 
  printing, with an empty string. 
-}
padSideLine :: Int -> Row -> Row
padSideLine longestLine line = (replicate numBlanks (-1)) ++ line
    where
        numBlanks = longestLine - length (line)

{-
  Generates a string for a size number. If it is equal to -1 (the 'null' value) then it is replaced with 
  a three character long empty string. Otherwise, the size is converted to a string and the string is padded 
  with a number of empty characters ' ' such that the whole string is three characters long.
-}
convertSizeToString :: Int -> String
convertSizeToString x = 
    if x == -1 
        then "   "
        else (show x) ++ (replicate (3 - length (show x)) ' ')

{- 
  Generates a string for a whole row of size numbers, by mapping convertSizeToString accross the row and 
  concatenating the resulting list of strings
-}
createSideLineString :: Row -> String
createSideLineString line = concat (P.map (convertSizeToString) line)

{- 
  Prints out the top lines of the grid (displaying the vertical size numbers). Starts by padding all of the lines
  using padSideLine. The padded lines are then transposed, taking them from their individual columns to rows which
  can be converted to a printable line. The number rows are passed into createTopLineString, turning each into 
  it's own string. A string for the gap before each row is then created, and appended to the start of each string.
  All the strings are then printed to screen, by having putStrLn mapped across the list of them.
-}
printTopLines numTopLines horizontalgap sizes = P.mapM_ (putStrLn) spacePaddedPrintLines
    where 
        paddedLines = P.map (padSideLine numTopLines) sizes
        transposedLines = D.transpose paddedLines
        createdPrintLines = P.map (createSideLineString) transposedLines
        gapstring = (replicate horizontalgap ' ')
        spacePaddedPrintLines = P.map (\x -> gapstring ++ x) createdPrintLines

{- 
  Converts the integers of the grid into their printable string representaton, where they are either blank or 
  a Square (representing a filled in space) The strings are 3 characters long, in accordance to the 'padding size'
  to display them all nicely.
-}
showSquare :: Int -> String
showSquare 0 = "   "
showSquare 1 = "■  " 

{-
  Converts a grid line to a string by mapping showSquare accross the values in the row and concatenating them.
-}
gridLinesToString gridLine = concat ( (P.map showSquare) gridLine )  

{- 
  Prints out the side lines (displaying the horizontal size numbers) and the lines of the grid next to each other.
  Starts by padding all of the side lines using padSideLine. createSideLineString is then mapped accross the padded 
  side lines, creating a list of side line strings. gridLinesToString is mapped across the lines of the grid, 
  creating a list of grid strings. The side line strings and grid line strings are then appended together using
  zipWith (++), and the resulting lines are printed to screen using putStrLn.
-}
printGridLines numSideLines sideLine grid = P.mapM_ (putStrLn) zs
   where
     paddedSideLines = P.map (padSideLine numSideLines) sideLine
     sideLineStrings = P.map (createSideLineString) paddedSideLines
     gridLineStrings = P.map (gridLinesToString) grid
     zs = P.zipWith (++) sideLineStrings gridLineStrings

{- 
  displayGrid takes two grids - the gameGrid (what the player is aiming towards, the final grid) and currentGrid 
  (the current state of the play grid). The game grid is converted into a tuple of size numbers using gridSizes.
  numSideLines and numTopLines are equal to the longest set of size numbers for the horizontal and verical set 
  respectively, and are used to pad the lines for display. The horizontalgap (representing the overall gap on the
  lefthand side is set to (numSideLines*3), as each string in the sideline is a total of 3 characters long. 

  The top lines of the grid, and the side lines and grid itself are then printed to screen using printTopLines 
  and printGridLines.
-}
displayGrid gameGrid currentGrid = do
    let sizes = gridSizes (gameGrid)
    let horizontalSizes = (fst sizes)
    let numSideLines = P.maximum (P.map (length) horizontalSizes)
    let verticalSizes = (snd sizes) 
    let numTopLines = P.maximum (P.map (length) verticalSizes)
    let horizontalgap = (numSideLines*3)
    printTopLines numTopLines horizontalgap verticalSizes
    printGridLines numSideLines horizontalSizes currentGrid

{- 
  Displays the completed nonogram grid for the image at a certain path.
-}
showCompleteGrid path = do  
    gameGrid <- generateGrid path
    displayGrid gameGrid gameGrid
{- 
  Checks if co-ordinate string is of a valid format, if not it will return a Left error message which can be caught and
  used. Otherwise it will convert the co-ordinate string to a tuple with two integers.
-}
checkCoOrdinates :: String -> Either String (Int,Int)
checkCoOrdinates coOrd = do
    case span (/=',') coOrd of
        ("",_) -> Left "Invalid Co-ordinate entry (use x,y)"
        (_,"") -> Left "Invalid Co-ordinate entry (use x,y)"
        (x,y) -> do
            let y' = (tail y)
            if all (isDigit) x && all (isDigit) y' 
                then do
                    let x' = P.read x :: Int
                    let y''= P.read y' :: Int
                    if (x' > gridDimensions || x'<1 || y''> gridDimensions || y''<1)
                        then do
                            let gdString = show gridDimensions -- Grid Dimensions string
                            Left ("Co-ordinates out of range, should be between (1,1) and (" ++ gdString ++ "," ++ gdString ++ ")") 
                        else Right (x',y'') 
                else Left "Co-ordinates invalid, ensure x and y are both integers" 

{- 
  flipSquareAt is a function which will take a grid, and an x,y co-ordinate and flip the value at that co-ordinate.
-}
flipSquareAt :: NonogramGrid -> (Int,Int) -> NonogramGrid
flipSquareAt currentGrid (x,y) = (ws ++ [newLine] ++ zs)
    where 
        (ws,  (z:zs))   = splitAt y currentGrid
        (wss, (z':zss)) = splitAt x z
        newVal = (z' + 1) `mod` 2
        newLine = (wss ++ [newVal] ++ zss)

{-
  The main 'game loop'. It displays the grid then checks if the grid has been completed or not. If the grid has been completed, a
  message is printed and the loop terminates. Otherwise, it takes in co-ordinates from the command line, checks the co-ordinates
  are valid and uses flipSquareAt to flip the square at that co-ordinate. Then, calls itself with the new grid, 
-}
playGrid gameGrid currentGrid = do
    displayGrid gameGrid currentGrid
    if gameGrid == currentGrid
      then do putStrLn "You completed the puzzle!" 
      else do 
        putStrLn "Co-ordinate to place / remove a square (enter 'x,y')"
        coOrd <- getLine
        case checkCoOrdinates coOrd of
            Left errorMsg -> do 
                putStrLn errorMsg
                playGrid gameGrid currentGrid 
            Right (x,y) -> do 
                let newGrid = flipSquareAt currentGrid (x-1,y-1) -- as it is indexed from 0 on the computer, need to reduce x and y by 1 
                playGrid gameGrid newGrid 
{-
  Main function that composes together all the other functions. Takes a path to a file from the command line, then generates a grid from
  the image at that file path. It then generates an empty grid of the same dimensions, and calls the playGrid function on the generated
  grid and the empty grid. 
-}
main = do
    putStrLn "Enter path to file to play picross with:"  
    path <- getLine
    gameGrid  <- generateGrid path
    let startGrid = replicate gridDimensions (replicate gridDimensions 0)
    playGrid gameGrid startGrid

