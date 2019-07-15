{-# LANGUAGE OverloadedStrings #-} 


module PositionMap where 

import qualified Data.Map as Map
import Data.Maybe(fromJust,isJust)
import SmallFunctions(average)
import Data.Text.Internal
import Data.String.Conversions (cs)
{- Every word in the String should be labeled with its position. 
  The difficulty here is that many words , e.g. names, places whatever, should always be looked at as a unit.
  So   the sentence "Max Planck's place of burial is the  Stadtfriedhof Göttingen" should be partitioned like
"(Max Planck,0) (place of burial,1) (is,2) (the,3) (Stadtfriedhof Göttingen,4)". Another difficulty is here to detect Max Planck's as Max Planck or that maybe words like
"Christian-Albrechts Universität" have a name + a  place in it, so there wouldn´t be just 1 Position. So maybe it is better to give every word a position and save positions of a name/place,etc.
seperated. Also there can be multiple occurences of names. So if we have a name and wanna save all information about position we need a tupel with personName as first
argument and a list of postions as second argument, e.g. (Max Planck, [7,37,117]) means that Max Planck is to find at position 7-8 , 37-38, 117-118... so the nummber gives us information about the
position of the first word of the first argument of the tupel.


---------------------------------------------------

Structure the String

Because we will have to go many times through the String we should pack it in a  datastructure in which we can easily search for certain names.
Yes, i am talking about searchtrees.


Idea: We need 2 trees. The first tree is sorted lexicographical and saves all of our positional tupels. I won`t  balance it in the first approach, more like insert every word one after the other
and hope that it is kinda balanced.

The second tree is sorted by the postion of the words.

That 2-Tree-Structure  gonna be  rly useful when we are seacrhing for certain word combinations like "Stadtfriedhof Göttingen". Then we can check in the first tree if the word "Stadtfriedhof"
is in the String in logarithmic searchtime (balanced SearchTree). Afterwards we have all positions where the word "Stadtfriedhof" appears. For example (Stadtfriedhof,[7,57,163]). Now we have to look up if at position 8,58,164
the word "göttingen" stands. Therefore we can easily search in our second tree. Again per search in logarithmic time. This way to search is rly efficient in the way that we have to search for words/phrases from our metadata
we get from our person name.For example again if we have found "max planck" in our String we get lots of linked information(words,phrases) , e.g. place of birth, from the wikidata article. And if we wanna check if those words/phrases
appear in our String, we can use this 2 tree-search to check whether the word/phrase is or is not in the String. Additionally we get the relative distance of the phrases and the names, which is needed for our rankings.



Max


-}

-- just some typesynonyms for our positions

type Pos = [Int]

type SinglePos = Int 
type NamePosition = Int 

type Distance  = Int 

type Distances = [Distance]

type Name      = String 

type RelatedString     = String
type RelatedStrings    = [RelatedString]



-- onlyWords
-- finds all space seperated words and deletes all symbols like ',' or '.' or e.g. the 's' at Max Planck's
onlyWords :: String -> [String]
onlyWords = words -- not yet defined 


-- number all words in a string

numberTheString :: String -> [(String,SinglePos)]
numberTheString str = let ws = words  str 
                          zipped = zip ws  [0 .. ]
                      in zipped -- map (\(w,n) -> ((cs w),n) ) zipped


-- construct the KVs


initPosKV :: String ->  Map.Map SinglePos String 
initPosKV str = let newKV = (Map.empty :: Map.Map SinglePos String)
                    numbered = numberTheString str
                in foldr (\(s,p) kv -> Map.insert p s kv ) newKV numbered 


collectSameStr :: String -> [(String,Pos)]
collectSameStr str = let numbered =  numberTheString str 
                     in  collect' numbered [] where 
                           collect' []     acc            = acc 
                           collect' (t:ts) acc            =  collect' ts (insert' t acc []) 
                           insert' t    []  accIns        = ((fst t,[snd t]) : accIns) 
                           insert' (s,p)  ((sI,ps):sss) accIns  | s == sI    = sss ++ ((s,(p:ps)) : accIns)  
                                                                | otherwise  = insert' (s,p) sss ((sI,ps):accIns)

initStrKV :: String ->  Map.Map String Pos
initStrKV str  = let newKV = (Map.empty :: Map.Map  String Pos)
                     numbered = collectSameStr str
                 in  foldr (\(s,ps) kv -> Map.insert s ps kv ) newKV numbered 
-----------------------------------------
--finding operations


--takes a single word and looks in the Stringsortedtree if 
-- the word is in it and returns if found the positions of the given word
--else returns Nothing
findWord :: String -> Map.Map String Pos ->   Pos 
findWord  str keyM = let ps = Map.lookup str keyM
                     in case ps of 
                                   Nothing        -> []
                                   (Just x)       -> x 



-- gets a position and returns the word at its position in the String if its exists
-- otherwise returns nothing
-- expects a positive number 
findWordAtPos :: SinglePos -> Map.Map SinglePos String -> Maybe String 
findWordAtPos pos keyM = Map.lookup pos keyM



-- in general we try to find a string in the String, which can be 1,2,3 .. words
-- so we need to combine our findword functons here
-- and the output is a positionlist of where to find the input string
-- empty lsit means the string is not in the String

findString :: String  -> Map.Map String Pos -> Map.Map SinglePos String ->  Pos 
findString strF stStr stPs 
  = let toFind     = onlyWords strF  -- function
        firstWord  = head toFind     
        firstPos   = findWord firstWord stStr 
        restWords  = tail toFind 
    in  case firstPos of 
          [] -> []
          ps  -> find' restWords ps 1 where 
            find' [] accPs cnt     = accPs 
            find' (w:ws) accPs cnt = let wPs = filter (\p -> isJust(findWordAtPos (p+cnt) stPs) && (w == fromJust(findWordAtPos (p+cnt) stPs)) ) accPs  
                                     in  find' ws wPs (cnt+1)    




-- distance functions

-- distance between 2 single position
distance2 :: SinglePos -> SinglePos -> Distance  
distance2 pos1 pos2 = abs (pos1 - pos2)


--example input : distancePosLists ([7,34,117] :: [Int]) ([17,60,278] :: [Int])
--  output [10,53,271,17,26,244,100,57,161]
-- so this functions gets all distances between every singlePos in the first Position and every singlePos in the second Position
-- meaning : we find the name "Max Planck" at positions [7,34,117] and the word "Göttingen at ([17,60,278], so all distances between max planck and göttingen would be
-- [10,53,271,17,26,244,100,57,161]  . Now we could just proceed with the shortest distance or with the average distance or with the amount of distances to influence our ranking later on
distancePosLists :: Pos -> Pos -> Distances 
distancePosLists ps1 ps2 = [distance2] <*> ps1 <*> ps2  

-- computes the distances between the given name and a given string, which may be related to the name
distancesBetweenWordNName :: NamePosition -> RelatedString ->  Map.Map String Pos -> Distances 
distancesBetweenWordNName nameP word strTree         = let psWord    = findWord word strTree  -- positions of the given word
                                                           distances = distancePosLists [nameP] psWord 
                                                       in distances

                               


{-
-- computes the distances between the given name and a given string, which may be related to the name
distancesBetweenWordNName :: Name -> RelatedString ->  Map.Map String Pos -> Map.Map SinglePos String -> Distances 
distancesBetweenWordNName name word strTree posTree = let psWord    = findString word strTree posTree -- positions of the given word
                                                          psName    = findString name strTree posTree --positions of the given name
                                                          distances = distancePosLists psName psWord 
                                                      in distances 

-}
distancesBetweenWordsNName :: NamePosition -> RelatedStrings -> Map.Map String Pos ->  [(RelatedString,Distances)]
distancesBetweenWordsNName nameP strings stringKM  =  foldr (\str acc -> (str,(distancesBetweenWordNName nameP str stringKM )) : acc ) [] strings