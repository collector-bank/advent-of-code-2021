import Data.List (subsequences, permutations, nub)
import Data.Maybe (listToMaybe, isJust, fromJust, mapMaybe)
import Debug.Trace (trace)

-- ref: https://codereview.stackexchange.com/questions/253400/split-string-by-delimiter-in-haskell
split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

type Vector = [Int]
type ScannerId = Int
data Scanner = Scanner { _scannerId :: Int, _beacons :: [Vector], _position :: Maybe Vector } deriving Show
type Axis = (Int,Int)
type CoordSys = [Axis]

parseScanners :: [String] -> [Scanner]
parseScanners [] = []
parseScanners (x:xs) =
    let
        id = read $ takeWhile  (/=' ') $ drop 12 x :: Int
        parseBeacon s = map (read :: String -> Int) (split ',' s)
        currData = takeWhile (/="") xs
        nextData = dropWhile (/="") xs
        curr = Scanner id (map parseBeacon currData) Nothing
        next =
            case nextData of
                (y:ys) -> parseScanners ys
                _ -> []
    in
        curr : next

vSub :: Vector -> Vector -> Vector
vSub = zipWith (-)

vAdd :: Vector -> Vector -> Vector
vAdd = zipWith (+)

transform :: CoordSys -> Vector -> Vector
transform [(xa,dx),(ya,dy),(za,dz)] v =
    let
        x = v !! xa
        y = v !! ya
        z = v !! za
    in
        [x*dx, y*dy, z*dz]
transform _ _ = error "invalid coordsys"

matches :: [Vector] -> [Vector] -> [Vector]
matches v1 v2 = [ v | v <- v1, v `elem` v2 ]

match :: [Vector] -> [Vector] -> Maybe ([Vector], Vector)
match beacons1 beacons2 =
    listToMaybe [ (beacons2'', delta) | 
        beacon1 <- beacons1,                               -- pick candidate for alignment in first set of beacons
        coordSys <- [
            [(x,dx),(y,dy),(z,dz)] |
            dx <- [1,-1],
            dy <- [1,-1],
            dz <- [1,-1],
            [x,y,z] <- permutations [0,1,2]
        ],
        let beacons2' = map (transform coordSys) beacons2,   -- change coordinate system (rotate and flip)
        beacon2' <- beacons2',                                -- pick candidate for alignment if second set of beacons
        let delta = vSub beacon1 beacon2',
        let beacons2'' = map (vAdd delta) beacons2',       -- shift beacon set 2 according to selected alignment
        let m = matches beacons1 beacons2'',
        length m >= 12
    ]

alignAll :: [Scanner] -> [Scanner]
alignAll [] = error "bah"
alignAll (x:xs) = alignAll' xs [x { _position = Just [0,0,0] }]
    where
        alignAll' :: [Scanner] -> [Scanner] -> [Scanner]
        alignAll' [] aligned = aligned
        alignAll' unaligned aligned =
            let
                align (Scanner id1 a _) (Scanner id2 b Nothing) =
                    case match a b of
                        Just (matches, pos) -> Just $ Scanner id2 matches (Just pos)
                        _ -> Nothing
                align _ _ = error "buh!"
                tryAlignOne [] _ = Nothing
                tryAlignOne (y:ys) x =
                    case align y x of
                        Nothing  -> tryAlignOne ys x
                        result -> result
                aligned' = aligned ++ mapMaybe (tryAlignOne aligned) unaligned
                unaligned' = let alignedIds' = map _scannerId aligned' in filter (flip notElem alignedIds' . _scannerId) unaligned
            in
                case aligned' of
                    [] -> error "ba!"
                    _  -> trace ("status: (" ++ (show . map _scannerId $ aligned') ++ "; " ++ (show . map _scannerId $ unaligned') ++ ")") 
                          alignAll' unaligned' aligned'


uniqueBeacons :: [Scanner] -> [Vector]
uniqueBeacons = nub . concatMap _beacons

hamming :: Vector -> Vector -> Int
hamming a b = sum $ zipWith (\x y -> abs (x - y)) a b

main = do
    scanners <- parseScanners . lines <$> readFile "input.txt"
    let alignedScanners = alignAll scanners
    print "Part 1, number of beacons:"
    print $ length $ uniqueBeacons alignedScanners  -- 434
    print "Part 2, max hamming distance"
    let distance = maximum [hamming (fromJust p1) (fromJust p2) |
           s1 <- alignedScanners,
           let p1 = _position s1,
           s2 <- alignedScanners,
           let p2 = _position s2]
    print distance  -- 11906
