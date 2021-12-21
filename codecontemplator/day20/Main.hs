import Data.Map (Map)
import qualified Data.Map as Map

data Image = Image { _pixels :: Map (Int,Int) Bool, _minX :: Int, _maxX :: Int, _minY :: Int, _maxY :: Int, _defaultValue :: Bool } deriving Show

type Table = Map Int Bool

charToBit :: Char -> Bool
charToBit = (=='#')

bitToChar :: Bool -> Char
bitToChar False = '.'
bitToChar True = '#'

parseInput :: [String] -> (Table, Image)
parseInput (x:_:xs) =
    let
        table = Map.fromList $ zip [0..] (map charToBit x)
        cells = Map.fromList $ [ ((j,i),charToBit c) | (j,r) <- zip [1..] xs, (i,c) <- zip [1..] r]
    in
        (table, Image cells 1 (length xs) 1 (length $ head xs) False)
parseInput _ = error "buh!"

binVecToInt :: [Bool] -> Int
binVecToInt = fst . foldr (\b (s,pos) -> ((if b then 2^pos else 0) + s, pos+1)) (0,0)

dim ::[(Int,Int)] -> (Int, Int, Int, Int)
dim keys =
    let
        xs = map snd keys
        ys = map fst keys
    in
        (minimum  xs, maximum xs, minimum ys, maximum ys)

kernel :: (Int,Int) -> [(Int,Int)]
kernel (j,i) = [(j+dj,i+di) | dj <- [-1..1], di <- [-1..1]]

coords :: Image -> [(Int,Int)]
coords (Image _ minX maxX minY maxY _) = [ (j,i) | j <- [minY-2..maxY+2], i <- [minX-2..maxX+2] ]

enhance :: Table -> Image -> Image
enhance table orig@(Image pixels minX maxX minY maxY defaultValue) =
    let
        lookupPixel k = Map.findWithDefault defaultValue k pixels
        folder index pixels' =
            let
                tableIndex = binVecToInt $ map lookupPixel (kernel index)
                (Just newValue) = Map.lookup tableIndex table -- lookup should never fail, table should be large enough
            in
                Map.insert index newValue pixels'

        pixels' = foldr folder Map.empty (coords orig)
        (minX',maxX',minY',maxY') = dim (Map.keys pixels')
        (Just defaultValue') = Map.lookup (binVecToInt $ replicate 9 defaultValue) table
    in
        Image pixels' minX' maxX' minY' maxY' defaultValue'

countLit :: Image -> Int
countLit img = length $ filter id $ Map.elems $ _pixels img

-- showImage :: Image -> [String]
-- showImage (Image pixels minX maxX minY maxY defaultValue) =
--     [ [ bitToChar (Map.findWithDefault defaultValue (j,i) pixels) | i <- [minX..maxX] ] | j <- [minY..maxY] ]


enhanceN :: Table -> Int -> Image -> Image
enhanceN t n img = iterate (enhance t) img !! n

main = do
    (table, image0) <- parseInput . lines <$> readFile "input.txt"
    putStrLn "Part 1:"
    let image2 = enhanceN table 2 image0    
    print $ countLit image2  -- 5846
    putStrLn "Part 2:"
    let image50 = enhanceN table 50 image0    
    print $ countLit image50  -- ?

