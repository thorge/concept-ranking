

module Position where 

import Data.List(sort,sortOn)
import Data.Maybe(fromJust,isJust)
import SmallFunctions(medianWithPos)
{- Every word in the text should be labeled with its position. 
  The difficulty here is that many words , e.g. names, places whatever, should always be looked at as a unit.
  So   the sentence "Max Planck's place of burial is the  Stadtfriedhof Göttingen" should be partitioned like
"(Max Planck,0) (place of burial,1) (is,2) (the,3) (Stadtfriedhof Göttingen,4)". Another difficulty is here to detect Max Planck's as Max Planck or that maybe words like
"Christian-Albrechts Universität" have a name + a  place in it, so there wouldn´t be just 1 Position. So maybe it is better to give every word a position and save positions of a name/place,etc.
seperated. Also there can be multiple occurences of names. So if we have a name and wanna save all information about position we need a tupel with personName as first
argument and a list of postions as second argument, e.g. (Max Planck, [7,37,117]) means that Max Planck is to find at position 7-8 , 37-38, 117-118... so the nummber gives us information about the
position of the first word of the first argument of the tupel.


---------------------------------------------------

Structure the text

Because we will have to go many times through the text we should pack it in a  datastructure in which we can easily search for certain names.
Yes, i am talking about searchtrees.


Idea: We need 2 trees. The first tree is sorted lexicographical and saves all of our positional tupels. I won`t  balance it in the first approach, more like insert every word one after the other
and hope that it is kinda balanced.

The second tree is sorted by the postion of the words.

That 2-Tree-Structure  gonna be  rly useful when we are seacrhing for certain word combinations like "Stadtfriedhof Göttingen". Then we can check in the first tree if the word "Stadtfriedhof"
is in the text in logarithmic searchtime (balanced SearchTree). Afterwards we have all positions where the word "Stadtfriedhof" appears. For example (Stadtfriedhof,[7,57,163]). Now we have to look up if at position 8,58,164
the word "göttingen" stands. Therefore we can easily search in our second tree. Again per search in logarithmic time. This way to search is rly efficient in the way that we have to search for words/phrases from our metadata
we get from our person name.For example again if we have found "max planck" in our text we get lots of linked information(words,phrases) , e.g. place of birth, from the wikidata article. And if we wanna check if those words/phrases
appear in our text, we can use this 2 tree-search to check whether the word/phrase is or is not in the text. Additionally we get the relative distance of the phrases and the names, which is needed for our rankings.



Max


-}

-- just some typesynonyms for our positions
type Pos = [Int]

type SinglePos = Int 

type Distance  = Int 

type Distances = [Distance]

type Name      = String 

type RelatedString     = String
type RelatedStrings    = [RelatedString]

-- our tree datatype where we will save the words and positions from the text as tupels
data SearchTree a = Empty | Node (SearchTree a) a (SearchTree a) 
 deriving Show 


foldSearchTree :: b -> (b -> a -> b -> b) -> SearchTree a ->  b
foldSearchTree empty node Empty                  = empty 
foldSearchTree empty node (Node t1 a t2)         = node (foldSearchTree empty node t1) a (foldSearchTree empty node t2)

--constructors
--------------------------------------------------

emptyTree = Empty 


--selectors  
------------------------------------------

treeSize :: SearchTree a -> Int 
treeSize  = foldSearchTree 0 (\n1 t n2 -> n1 + 1 + n2) 


treeDepth :: SearchTree a -> Int 
treeDepth =  foldSearchTree 0 (\n1 t n2 -> 1 + (max n1 n2)) 
-- tree operations
----------------------------------------


-- insert one tupel of a word and its positions in a tree which is sorted by the strings
-- we need that to initialize our first tree later 
insertByString :: (String,SinglePos) -> SearchTree (String,Pos) -> SearchTree (String,Pos)
insertByString (str,pos) Empty                                          = Node Empty (str,[pos]) Empty 
insertByString tupel@(strI,posI) (Node t1 (str,ps) t2 )   | strI < str  = Node (insertByString tupel t1) (str,ps)         t2 
                                                          | strI > str  = Node t1                        (str,ps)         (insertByString tupel t2) 
                                                          | otherwise   = Node t1                        (str,(posI:ps))  t2  
 
-- insert one single position with its string into a st which is sorted by the positions
-- because there is every position only once we can choose SearchTree (String,SinglePos) as a suiting data type to save the given input later on in our  second tree
insertByPos :: (String,SinglePos) -> SearchTree (String,SinglePos)  -> SearchTree (String,SinglePos)
insertByPos (str,pos) Empty                                             = Node Empty (str,pos) Empty 
insertByPos tupel@(strI,posI) (Node t1 (str,pos) t2 )     | posI < pos  = Node (insertByPos tupel t1) (str,pos)  t2 
                                                          | posI > pos  = Node t1                     (str,pos)  (insertByPos tupel t2) 
                                                          | otherwise   = Node t1                     (str,pos)  t2  -- this case shouldnt exist


--finding operations


--takes a single word and looks in the Stringsortedtree if 
-- the word is in it and returns if found the positions of the given word
--else returns Nothing
findWord :: String -> SearchTree (String,Pos) ->  Maybe Pos 
findWord  _  Empty = Nothing 
findWord  strF (Node t1 (str,ps) t2) | strF < str = findWord strF t1 
                                     | strF > str = findWord strF t2
                                     | otherwise  = Just ps  -- word found

-- gets a position and returns the word at its position in the text if its exists
-- otherwise returns nothing
-- expects a positive number 
findWordAtPos :: SinglePos -> SearchTree (String,SinglePos) ->  Maybe String 
findWordAtPos  _  Empty = Nothing 
findWordAtPos  posF (Node t1 (str,pos) t2) | posF < pos = findWordAtPos posF t1 
                                           | posF > pos = findWordAtPos posF t2
                                           | otherwise  = Just str  -- word found 


-- in general we try to find a string in the text, which can be 1,2,3 .. words
-- so we need to combine our findword functons here
-- and the output is a positionlist of where to find the input string
-- empty lsit means the string is not in the text
findString :: String -> SearchTree (String,Pos) -> SearchTree (String,SinglePos) ->  Pos 
findString strF stStr stPs 
  = let toFind     = onlyWords strF  -- function
        firstWord  = head toFind     
        firstPos   = findWord firstWord stStr 
        restWords  = tail toFind 
    in  case firstPos of 
         Nothing -> []
         Just ps -> find' restWords ps 1 where 
          find' [] accPs cnt     = accPs 
          find' (w:ws) accPs cnt = let wPs = filter (\p -> isJust(findWordAtPos (p+cnt) stPs) && (w == fromJust(findWordAtPos (p+cnt) stPs)) ) accPs  
                                   in  find' ws wPs (cnt+1)    




-----------------------------------------
--sample trees

empty= Empty

t1:: SearchTree (String,Pos)
t1 = Node Empty ("moin",[7]) (Node Empty ("hi",[53]) (Node (Node Empty ("servus",[10]) Empty ) ("hallo",[2]) Empty)) 


-- init trees

------------------------------------

-- number all words in a string

numberTheString :: String -> [(String,SinglePos)]
numberTheString str = let ws = words str 
                      in zip ws  [0 .. ]


                     






-- onlyWords
-- finds all space seperated words and deletes all symbols like ',' or '.' or e.g. the 's' at Max Planck's
onlyWords :: String -> [String]
onlyWords = words -- not yet defined 
-----------------------------------------------------------------------------

initStringTree :: String -> SearchTree (String,Pos)
initStringTree str =  let numbered       = numberTheString str 
                          sortedbyStr    = sortOn (\(str,pos) -> str) numbered
                      in  init' sortedbyStr (Empty :: SearchTree (String,Pos)) where 
  init' [] accTree = accTree 
  init' ws accTree = let (med,med_pos)  = medianWithPos ws 
                         leftMed  = take   med_pos      ws
                         rightMed = drop  (med_pos+1)   ws
                         newTree  = insertByString med accTree
                     in  init' rightMed (init' leftMed newTree)


initPosTree :: String -> SearchTree (String,SinglePos)
initPosTree str    =  let numbered       = numberTheString str 
                          sortedbyStr    = sortOn (\(str,pos) -> pos) numbered -- sorted anyways
                      in  init' sortedbyStr (Empty :: SearchTree (String,SinglePos)) where 
  init' [] accTree = accTree 
  init' ws accTree = let (med,med_pos)  = medianWithPos ws 
                         leftMed  = take   med_pos      ws
                         rightMed = drop  (med_pos+1)   ws
                         newTree  = insertByPos med accTree
                     in  init' rightMed (init' leftMed newTree)



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

-- returns the shortest distance between the positions of two words
shortestOfDistances :: Pos -> Pos -> Distance 
shortestOfDistances ps1 ps2 = minimum  $ distancePosLists ps1 ps2





-- computes the distances between the given name and a given string, which may be related to the name
distancesBetweenWordNName :: Name -> RelatedString -> SearchTree (String,Pos) -> SearchTree (String,SinglePos) -> Distances 
distancesBetweenWordNName name word strTree posTree = let psWord    = findString word strTree posTree -- positions of the given word
                                                          psName    = findString name strTree posTree --positions of the given name
                                                          distances = distancePosLists psName psWord 
                                                      in distances 
                                                      

-- the function above gets 1 name and 1 possible relatedword
-- now we need a function to check 1 name and many possible related words
-- the result must provide the information which relatedstring  has which distances to the given name 

distancesBetweenWordsNName :: Name -> RelatedStrings -> SearchTree (String,Pos) -> SearchTree (String,SinglePos) -> [(RelatedString,Distances)]
distancesBetweenWordsNName = undefined 