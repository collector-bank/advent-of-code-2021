import Data.Bifunctor
import Data.List (nub, sort)

flipIf :: Int -> Int -> Int
flipIf m x = if x > m then m - (x - m)  else x

flipX :: Int -> (Int,Int) -> (Int,Int)
flipX m = first (flipIf m)

flipY :: Int -> (Int,Int) -> (Int,Int)
flipY m = second (flipIf m)

parseFlip :: String -> (Char, Int)
parseFlip s =
    let
        axis = s !! 11
        coord = read $ drop 13 s
    in
        (axis,coord)

parseCoord s = (read . takeWhile (/= ',') $ s, read . tail . dropWhile (/= ',') $ s)

applyFlip :: [(Int,Int)] ->  (Char, Int) -> [(Int,Int)]
applyFlip coords (axis, m) =
    if axis == 'x' then
        nub $ map (flipX m) coords
    else
        nub $ map (flipY m) coords

getDim :: [(Int,Int)] -> ((Int,Int),(Int,Int))
getDim coords =
    let minx = minimum $ map fst coords
        maxx = maximum  $ map fst coords
        miny = minimum $ map snd coords
        maxy = maximum  $ map snd coords
    in
        ((minx, miny), (maxx, maxy))

toMatrix :: [(Int,Int)] -> [String]
toMatrix coords =  do
    let ((minx, miny), (maxx, maxy)) = getDim coords
    map (\r -> [if (c,r) `elem` coords then '#' else '.' | c <- [minx..maxx]]) [miny..maxy]

main = do
    ls <- lines <$> readFile "input.txt"
    let coords = map parseCoord $ takeWhile (/="") ls
    let flips = map parseFlip $ tail $ dropWhile  (/="") ls

    let flippedOnce = applyFlip coords (head flips)
    print "result 1:"
    print (show . length $ flippedOnce)    -- 669

    let matrix = toMatrix $ foldl applyFlip coords flips
    print "result 2:"
    mapM_ print matrix -- UEFZCUCJ
