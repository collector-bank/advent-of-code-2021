{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Rule = ((Char,Char),Char)
type RuleMap = Map (Char,Char) Char

parseFile :: [String] -> (String, [Rule])
parseFile (r1:_:rs) =
  let
    parseRule :: String -> Rule
    parseRule [c1,c2,' ','-','>',' ',c3] = ((c1,c2), c3)
  in
    (r1, map parseRule rs)

type State = Map (Char,Char) Int

expand :: RuleMap -> State -> State
expand ruleMap state1 = 
  let 
    folder (x1,x2) state = 
        case Map.lookup (x1,x2) ruleMap of
          Just c -> 
              case Map.lookup (x1,x2) state1 of
                  Nothing -> state
                  Just count ->
                        Map.insertWith (+) (x1,c) count $
                        Map.insertWith (+) (c,x2) count $
                        Map.insertWith (flip (-)) (x1,x2) count $
                        state
          Nothing -> state
  in
    foldr folder state1  (Map.keys state1)

expandN ::  RuleMap -> State -> [State]
expandN ruleMap = iterate (expand ruleMap)

summarize :: State -> Map Char Int
summarize state =
  let
    folder (x1,x2) (s1,s2) =     
      let 
        count = fromJust . Map.lookup (x1,x2) $ state
        s1' = Map.insertWith (+) x1 count s1
        s2' = Map.insertWith (+) x2 count s2
      in
        (s1',s2')
    (cfst, csnd) = foldr folder (Map.empty,Map.empty) (Map.keys state)
  in
    Map.unionWith max cfst csnd

main = do
    (seedi, rules)  <- parseFile . lines <$> readFile "input.txt"
    let ruleMap = foldr (\(a,b) s -> Map.insert a b s) Map.empty rules

    let pairs = zip seedi (tail seedi)
    let seed = foldr (\e s -> Map.insertWith (+) e 1 s) Map.empty pairs

    let solve stepCount = do
        let states = take stepCount (expandN ruleMap seed)
        let state = last states
        let x = sort $ map snd $ Map.toList (summarize state)
        print $ last x - head x

    print "result 1:"
    solve 11  -- 3406
    print "result 2:"
    solve 41  -- 3941782230241


