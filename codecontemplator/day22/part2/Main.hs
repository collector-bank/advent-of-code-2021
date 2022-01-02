-- https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-MArray.html
-- https://news.ycombinator.com/item?id=15017853
{-# LANGUAGE  TemplateHaskell #-}

module Main where

import Control.Lens
import Data.List.Split (splitOn)
import Text.Regex.Posix
import Data.List (sort, nub, elemIndex)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int (Int16)
import Data.Array.MArray
import Data.Array.IO
import Data.IORef

data Range i = Range { _from :: i, _to :: i } deriving Show
data Cube i a = Cube { _xr :: Range i, _yr :: Range i, _zr :: Range i, _value :: a } deriving Show
type StdRange = Range Int
type Idx = Int16
type Table = Map Int16 Int
type Matrix3d = IOArray (Int16,Int16,Int16) Bool 

makeLenses ''Range
makeLenses ''Cube

parseLine :: String -> Cube Int Bool
parseLine s =
    let
        parseRange :: String -> StdRange
        parseRange sr = let [[_,from,to]] = sr =~ ".=(-?[0-9]+)\\.\\.(-?[0-9]+)" :: [[String]] in Range (read from) (read to + 1)
        [onOff,ranges] = splitOn " " s
        [xRange,yRange,zRange] = splitOn "," ranges
    in
        Cube (parseRange xRange) (parseRange yRange) (parseRange zRange) (onOff == "on")

mkTable :: [Int] -> Table
mkTable t = Map.fromList $ zip [0..] t

uniqueSorted :: Foldable t => Getting StdRange s StdRange -> t s  -> [Int]
uniqueSorted focus = sort . nub . concatMap (collapseRange . view focus)

collapseRange :: StdRange -> [Int]
collapseRange r = [ _from r, _to r]

mkIndexed :: [Int] -> [Int] -> [Int] -> Cube Int a -> Cube Idx a
mkIndexed tx ty tz (Cube xr yr zr v) = Cube (convert xr tx) (convert yr ty) (convert zr tz) v
    where
        convert (Range from to) table = Range (idxOf from table) (idxOf to table)
        idxOf x table = fromIntegral . fromJust . elemIndex x $ table

mkMatrix3d :: Int -> Int -> Int -> IO Matrix3d
mkMatrix3d sx sy sz = 
    let 
        indexFrom = (0,0,0) :: (Int16,Int16,Int16)
        indexTo = (fromIntegral sx, fromIntegral sy, fromIntegral sz) :: (Int16,Int16,Int16)
    in        
        newArray (indexFrom, indexTo) False

update :: Table -> Table -> Table -> Cube Idx Bool -> Matrix3d -> IORef Int -> IO ()
update tx ty tz (Cube (Range xMin xMax) (Range yMin yMax) (Range zMin zMax) v) m  count =
    sequence_ $ do
        xi <- [xMin..xMax-1]
        yi <- [yMin..yMax-1]
        zi <- [zMin..zMax-1]
        return $ do 
            vo <- readArray m (xi, yi, zi)
            writeArray m (xi, yi, zi) v
            let (x1,y1,z1) = (lookup tx xi, lookup ty yi, lookup tz zi)
            let (x2,y2,z2) = (lookup tx (xi + 1), lookup ty (yi + 1), lookup tz (zi + 1))
            let size = (x2 - x1) * (y2 - y1) * (z2 - z1)
            case (vo, v) of
                (True, False) -> modifyIORef count (\c -> c - size)
                (False, True) -> modifyIORef count (\c -> c + size)
                _ -> return ()
    where lookup t i = fromJust $ Map.lookup i t 

main :: IO ()
main = do
    input <- map parseLine . filter (\(x:_) -> x /= '#') . lines <$> readFile "input.txt"
    let (xs, ys, zs) = (uniqueSorted xr input, uniqueSorted yr input, uniqueSorted zr input)
    let (tx, ty, tz) = (mkTable xs, mkTable ys, mkTable zs)
    let indexedCubes = map (mkIndexed xs ys zs) input

    space <- mkMatrix3d (length xs - 1) (length ys - 1) (length zs - 1)
    count <- newIORef 0

    sequence_ $ do
        cube <- indexedCubes      
        return $ do 
            tmp <- readIORef count
            putStrLn ("processing " ++ show cube ++ ", " ++ show tmp)
            update tx ty tz cube space count

    print "processing done"

    result <- readIORef count
    print result  -- 1187742789778677
