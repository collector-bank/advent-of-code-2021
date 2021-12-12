{-# LANGUAGE DeriveFunctor #-}

import Data.Set (Set)
import qualified  Data.Set as Set
import Data.Map (Map)
import qualified  Data.Map as Map
import Data.Maybe (Maybe)
import qualified  Data.Maybe as Maybe
import Data.Char (toLower)
import Data.Tuple (swap)
import Text.Printf (printf)

type Coalgebra f a = a -> f a
type Algebra f a = f a -> a

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

data Cave = Start | End | Other String deriving (Eq, Ord)

{-
instance Show Cave where
    show Start = "start"
    show End = "end"
    show (Other x) = x
-}

isSmall :: Cave -> Bool
isSmall (Other caveId) = caveId == map toLower caveId
isSmall _ = True

class CaveStore store where
    empty :: store
    isForbidden :: Cave -> store -> Bool
    register :: Cave -> store -> store

newtype VisitSmallCavesOnceOnly = VisitSmallCavesOnceOnly (Set Cave)

instance CaveStore VisitSmallCavesOnceOnly where
    empty = VisitSmallCavesOnceOnly Set.empty     
    isForbidden c (VisitSmallCavesOnceOnly s) = Set.member c s
    register cave (VisitSmallCavesOnceOnly s) = VisitSmallCavesOnceOnly $ if isSmall cave then Set.insert cave s else s

data AllowOneSmallCaveTwice = AllowOneSmallCaveTwice Bool (Set Cave)

instance CaveStore AllowOneSmallCaveTwice where
    empty = AllowOneSmallCaveTwice False Set.empty     
    isForbidden c (AllowOneSmallCaveTwice exceptionMade s) = Set.member c s && (exceptionMade || c == Start)
    register cave (AllowOneSmallCaveTwice exceptionMade s) = 
            case (isSmall cave, Set.member cave s) of
                (False, _    ) -> AllowOneSmallCaveTwice exceptionMade s
                (True , True ) -> AllowOneSmallCaveTwice True          s
                (True , False) -> AllowOneSmallCaveTwice exceptionMade (Set.insert cave s)

data Seed store = Seed { _current :: Cave, _continuations :: Map Cave [Cave], _forbidden :: store }

data SearchTreeF a = Finish | DeadEnd | Checkpoint Cave [a] deriving Functor

buildSearchTree :: CaveStore store => Coalgebra SearchTreeF (Seed store)
buildSearchTree (Seed End _ _) = Finish
buildSearchTree (Seed cave continuations store) =
    if isForbidden cave store then
        DeadEnd
    else
        Checkpoint cave 
            [  Seed cave' continuations (register cave store) | 
               cave' <- concat . Maybe.maybeToList $ Map.lookup cave continuations ]

type Path = [Cave]

getPaths :: Algebra SearchTreeF [Path]
getPaths DeadEnd = []
getPaths Finish = [[End]]
getPaths (Checkpoint cave paths) = map (cave:) (concat paths)

parseLine :: String -> (Cave, Cave)
parseLine s =
    let
        startName   = takeWhile        (/= '-') s
        endName     = tail $ dropWhile (/= '-') s
        toCave name =
            case name of
                "start" -> Start
                "end"   -> End
                _       -> Other name
    in
        (toCave startName, toCave endName)

main = do
    segments <- map parseLine . lines <$> readFile "input.txt"
    
    let undirected = segments <> map swap segments
    let graph      = foldr (\(a,b) s -> Map.insertWith (<>) a [b] s) Map.empty undirected
    let mkSeed     = Seed Start graph

    let result1 = hylo getPaths buildSearchTree $ mkSeed (empty :: VisitSmallCavesOnceOnly)
    printf "result1 = %d\n" (length result1)  -- 4495
    
    let result2 = hylo getPaths buildSearchTree $ mkSeed (empty :: AllowOneSmallCaveTwice)
    printf "result2 = %d\n" (length result2)  -- 131254
