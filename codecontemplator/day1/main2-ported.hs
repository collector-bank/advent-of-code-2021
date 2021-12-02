-- port of https://github.com/bjorkstromm/AdventOfCode2021/blob/main/day1.fsx to haskell

import Data.Function ((&))
import Data.Functor ((<&>))

(|>) = (&)
(||>) = (<&>)

windowed :: Int -> [a] -> [[a]]
windowed size ls =
   case ls of
      [] -> []
      x:xs ->
         if length ls >= size then
            (take size ls) : windowed size xs
         else windowed size xs

data Direction = Increased | Decreased | Unchanged deriving Eq

getMeasurements filename =
    readFile filename 
    ||> lines 
    ||> fmap (read :: String -> Int)


processMeasurements measurements =
    let 
        getDirection (a, b) =
            if a > b then Decreased
            else if a < b then Increased
            else Unchanged

        pairwise xs = zip xs (tail xs)
        
        tail' =
            measurements
            |> pairwise
            |> fmap (\(a, b) -> (b, getDirection (a, b)))

        head' = (measurements |> head, Unchanged)
    in
        head':tail'


-- Part 1 (1298)
getIncreases filename =
    filename
    |> getMeasurements
    ||> processMeasurements
    ||> filter (\(a, b) -> b == Increased)
    ||> length

-- Part 2 (1248)
processWindowed measurements =
    measurements
    |> windowed 3
    |> map sum
    |> processMeasurements

getIncreasesWindowed filename =
    filename
    |> getMeasurements
    ||> processWindowed
    ||> filter (\(a, b) -> b == Increased)
    ||> length
