import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Control.Monad (foldM)
import qualified Data.Ord

data AmphipodType = A | B | C | D deriving (Show, Eq, Ord)

energyPerStep :: AmphipodType -> Int
energyPerStep A = 1
energyPerStep B = 10
energyPerStep C = 100
energyPerStep D = 1000

type Energy = Int
type Position = (Int,Int)
data Cell = Wall | Hallway Bool | SideRoomA | SideRoomB | SideRoomC | SideRoomD deriving Show
type Board = Position -> Cell
type Amphipod = (Position,AmphipodType)
data GameState = GameState { _board :: Board, _pods :: [Amphipod], _energy :: Energy }

instance Show GameState where
    show (GameState board pods e) =
            unlines $
            [
                "energy: " ++ show e,
                "pods:" ++ show pods
            ] ++
            [
                [ x |
                    i <- [0..12],
                    let a = (\(_,t) -> head (show t)) <$> List.find (\(p,_) -> p == (i,j)) pods,
                    let b = case board (i,j) of
                                Wall -> '#'
                                _ -> '.',
                    let x = Maybe.fromMaybe b a
                ] |
                j <- [0..6]
            ]

isFree :: Position -> [Amphipod] -> Bool
isFree p = all (\(p',_) -> p /= p')

isHallway :: Cell -> Bool
isHallway (Hallway _) = True
isHallway _ = False

isSideRoom :: Cell -> Bool
isSideRoom SideRoomA = True
isSideRoom SideRoomB = True
isSideRoom SideRoomC = True
isSideRoom SideRoomD = True
isSideRoom _ = False

isImmediatelyOutsideAnyRoom :: Cell -> Bool
isImmediatelyOutsideAnyRoom (Hallway result) = result
isImmediatelyOutsideAnyRoom _ = False

sideRoomTarget :: Cell -> Maybe AmphipodType
sideRoomTarget SideRoomA = Just A
sideRoomTarget SideRoomB = Just B
sideRoomTarget SideRoomC = Just C
sideRoomTarget SideRoomD = Just D
sideRoomTarget _ = Nothing

noForeign :: Board -> AmphipodType -> [Amphipod] -> Bool
noForeign board at xs =
    null
        [ () |
            (p,t) <- xs,
            t /= at,
            let sideRoom = sideRoomTarget (board p),
            sideRoom == Just at
        ]

isDestinationAndReady :: Board -> Cell -> AmphipodType -> [Amphipod] -> Bool
isDestinationAndReady board cell at rest =
    sideRoomTarget cell == Just at &&
    noForeign board at rest

targetX :: AmphipodType -> Int
targetX t = case t of A -> 3; B -> 5; C -> 7; D -> 9

lowerBoundEnergyToMoveToHomeAll :: GameState -> Int
lowerBoundEnergyToMoveToHomeAll gs =
    let lowerBoundEnergyToMoveToHome (p@(x,_),t) = abs (x - targetX t) * energyPerStep t
    in sum $ map lowerBoundEnergyToMoveToHome (_pods gs)

canMoveIntoTargetSideRoom :: Board -> Amphipod -> [Amphipod] -> Bool
canMoveIntoTargetSideRoom board (p@(x,_),t) rest =
    let
        xt = targetX t
        xs
          | xt < x = [xt..x-1]
          | xt > x = [x+1..xt]
          | otherwise = []
        hallwayPositions = [ (i,1) | i <- xs ]
    in
        isHallway (board p) &&
        noForeign board t rest &&
        all (`isFree` rest) hallwayPositions

finishSelected :: Board -> Amphipod -> [Amphipod] -> (Energy,Position)
finishSelected board selected@(p@(x,_),t) rest =
    let
        xt = targetX t
        xs
          | xt < x = [xt..x-1]
          | xt > x = [x+1..xt]
          | otherwise = []
        hallwayPositions = [ (i,1) | i <- xs ]
        sideRoomPositions = takeWhile (\p -> isSideRoom (board p) && isFree p rest) [ (xt,j) | j <- [2..] ]
        energyDelta = (length hallwayPositions + length sideRoomPositions) * energyPerStep t
        p' = last sideRoomPositions
    in
        (energyDelta, p')

hallwayTargetPositions :: Board -> Amphipod -> [Amphipod] -> [(Energy, Position)]
hallwayTargetPositions board (p@(x,y),t) rest =
    let
        eps = energyPerStep t
        up = [ (x,j) | j <- [y-1,y-2..1] ]
        numUp = length up
        energy = (\horizontalSteps -> (horizontalSteps + numUp) * eps) <$> [1..]
        isAboveSideRoom p = isImmediatelyOutsideAnyRoom (board p)
        left = filter (not . isAboveSideRoom . snd) $ zip energy (takeWhile (`isFree` rest) [ p | i <- [x-1,x-2..1], let p = (i,1) ])
        right = filter (not . isAboveSideRoom . snd) $ zip energy (takeWhile (`isFree` rest) [ p | i <- [x+1..11], let p = (i,1) ])
    in
        if all (`isFree` rest) up then
            left <> right
        else
            []

step :: GameState -> [GameState]
step gs@(GameState board pods energy) =
    let        
        s2h =
            [ -- sideroom to hallway 
                GameState board (selected':pods') (energy+deltaEnergy) |
                selected@(p,t) <- pods,
                let cell = board p,
                let pods' = List.delete selected pods,
                isSideRoom cell,
                not(isDestinationAndReady board cell t pods'),  -- do not move away from sideroom if done
                (deltaEnergy,p') <- hallwayTargetPositions board selected pods',
                let selected' = (p',t)
            ]
        h2s = 
            [ -- hallway to sideroom
                GameState board (selected':pods') (energy+deltaEnergy) |
                selected@(p,t) <- pods,
                let cell = board p,
                let pods' = List.delete selected pods,
                isHallway cell,
                canMoveIntoTargetSideRoom board selected pods',
                let (deltaEnergy, p') = finishSelected board selected pods',
                let selected' = (p',t)
            ]
    in
        case h2s of 
            [] -> s2h
            _ -> h2s  -- if we can finish some, do that!

isDone :: GameState -> Bool
isDone (GameState board xs _) = all (\(p,t) -> sideRoomTarget (board p) == Just t) xs

data Topped x = Val x | Top deriving (Show, Eq, Ord)

searchDfs :: GameState -> Topped Energy -> IO (Topped Energy)
searchDfs gs knownBest = do
    let curEnergy = Val (_energy gs) 
    if knownBest < curEnergy then do
        return knownBest
    else
        if isDone gs then do
            putStrLn $ "new best solution found, energy = " ++ show curEnergy
            return curEnergy
        else  
            let children = 
                        [ gsChild |
                            gsChild <- step gs,
                            Val (_energy gsChild + lowerBoundEnergyToMoveToHomeAll gsChild) < knownBest
                        ]
                folder :: Topped Energy -> GameState -> IO (Topped Energy)
                folder knownBest gsChild = do
                    candidate <- searchDfs gsChild knownBest
                    return $ min knownBest candidate
            in
                foldM folder knownBest children

parseGameState :: [String] -> GameState
parseGameState rows =
    let
        boardMap =
            Map.fromList
                [
                    ((i,j), cell) |
                    (j,row) <- zip [0..] rows,
                    (i,ch) <- zip [0..] row,
                    let cell = case ch of
                                    '#' -> Wall
                                    ' ' -> Wall
                                    _ | j == 1 -> Hallway (i == 3 || i == 5 || i == 7 || i == 9)
                                    _ | i == 3 -> SideRoomA
                                    _ | i == 5 -> SideRoomB
                                    _ | i == 7 -> SideRoomC
                                    _ | i == 9 -> SideRoomD
                                    _ -> error "duh?"
                ]
        board index = Maybe.fromMaybe Wall (Map.lookup index boardMap)
        amphipods =
            [
                ((i,j),t) |
                (j,row) <- zip [0..] rows,
                (i,ch) <- zip [0..] row,
                ch `elem` ['A','B','C','D'],
                let t = case ch of 'A' -> A; 'B' -> B; 'C' -> C; 'D' -> D; _ -> error "doh?"
            ]
    in
        GameState board amphipods 0

part1 :: IO ()
part1 = do
    initialState <- parseGameState . lines <$> readFile "input_part1.txt"
    result <- searchDfs initialState Top
    print result  -- 16508

part2 :: IO ()
part2 = do
    initialState <- parseGameState . lines <$> readFile "input_part2.txt"
    result <- searchDfs initialState Top
    print result  -- 43626

main :: IO ()
main = part1 >> part2
