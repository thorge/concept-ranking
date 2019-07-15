{-# LANGUAGE OverloadedStrings #-} 


module ComputeProbs where 


import PositionMap
import SmallFunctions 
import qualified Data.Map as Map
import Json 
import Data.Text.Internal
import Data.List(sort,sortOn,nub)
import Data.String.Conversions (cs)
{-

now we need to use the functions and datastructures from PositionMap in a useful way to compute probabilities , which 
should identify if a person found in the text matches with the found person in the wikidata article/metadatas
Therefore in the first step we use the minimum distance of a name and its related string and the median distance.
Average distance makes no sense, e.g.  Max Planck was born in Kiel .... #10000 words  and more.. Max Planck .. . 
In our first approach we will weight both minimum and median equally. 
 -}



type WikidataID = String 

type Prop = String 

type PropValue = String 

type PropsValues = [(Prop,PropValue)]

type MetaWord = String 

type Occurence = Int 

type Occurs = (OriginalName -> Int)



initOccurs :: Occurs 
initOccurs = \oname -> 1
-- type Points = Float 


-- compute for every found name in the text itr rankings 

numberOriginalNames  :: [PersonInText] -> [PersonInText]
numberOriginalNames ps = number' ps initOccurs where 
  number' [] _        = []
  number' (p:ps) occ  = let oname   = getOriginalName p 
                            occurs  = occ oname 
                            newName =  (show occurs) ++ ". " ++ oname
                            newP    = setOriginalName newName p 
                            newOcc  = \o -> if o == oname then ((occ o) + 1) 
                                                          else occ o 
                        in newP : number' ps newOcc


zipWithOccurence :: Eq a => [a] -> [(a,Int)]
zipWithOccurence as = zip' (\x -> 0) as where 
  zip' f []       = []
  zip' f (a:as)   = let n      = f a 
                        newF   = \x -> if x == a then ((f a) + 1) else f a 
                    in (a,n) : zip' newF as 

computePoints :: QueryRes ->  [(OriginalName,[(WikiLink,Points)])] 
computePoints qRes = let inputTxt   = getTxt qRes 
                         nameList   =  getPersons qRes 
                         numbered   =   zipWithOccurence nameList 
                         stringTree = initStrKV inputTxt 
                         posTree    = initPosKV inputTxt -- unneccessary 
                     in map(computeOne stringTree posTree) numbered 


computeOne ::   Map.Map String Pos ->  Map.Map SinglePos String -> (PersonInText,Int) -> (OriginalName,[(WikiLink,Points)])
computeOne strT posTr personInt  = let oName           = getOriginalName $ fst personInt  
                                       positions       = findString oName strT posTr
                                       occurence       = snd personInt 
                                       singlePos       = positions !! occurence 
                                       wikiPersonList  =  getWikiPersons $ fst personInt  
                                       ranking = map (getRanking strT singlePos) wikiPersonList
                                       zipped  = (oName,ranking)   
                                   in zipped 

getRanking :: Map.Map String Pos -> SinglePos ->  WikiPerson -> (WikiLink,Points)
getRanking strTr pos person  = let linkedWords = getWikiWords person 
                                   wikiLink    = getWikiLink person 
                                   wikiName    = getWikiName person
                                   points      = foldr (\lWord acc -> (pointsfor1Word lWord pos strTr) + acc) 0 linkedWords 
                                   linkNName   = wikiName ++ " : " ++ wikiLink
                               in (linkNName, cs $ show points) 


pointsfor1Word :: String -> SinglePos -> Map.Map String Pos -> Float 
pointsfor1Word linkWord namePos strTr  = let linkWordPs = findWord   linkWord strTr 
                                             distances  =  map (distance2 namePos) linkWordPs
                                         in case distances of 
                                                              [] -> 0
                                                              _  -> comp' (minimum distances) (median $ sort distances) where 
                                                                      comp' mini1 medi1 = let min1   = fromInteger $ toInteger mini1
                                                                                              med1   = fromInteger $ toInteger medi1
                                                                                              points = 1.0 - (0.5* (min1**2) / 10000.0  + 0.5 * (med1**2) / 10000.0)
                                                                                           in if (points > 0) then points
                                                                                                              else 0 


                                            
  

{-
-- now we dont notice which Property we are looking on, we assume that every chosen property has the same value 
pointsNameAndMetaData :: WikidataID -> Name -> PropsValues -> Map.Map String Pos -> Map.Map SinglePos String -> (WikidataID,Name,Points) 
pointsNameAndMetaData wikiID name pvs sp ss  
  = let onlyValues       = foldr (\(p,v) acc -> v : acc )  [] pvs
        propsNDistances  = distancesBetweenWordsNName name onlyValues sp ss 
        onlyDistances    = foldr (\(p,ds) acc -> ds : acc )  [] propsNDistances
    in points'  onlyDistances  (0 :: Float) where 
         points'   []      accPoints     = (wikiID,name,accPoints)
         points'   ([]:ds) accPoints     = points' ds accPoints
         points'   (d:ds)  accPoints     = let mini = minimum d  
                                               med  = median $ sort d  
                                           in points' ds (accPoints + comp' mini med )
         comp' mini1 medi1 = let min1   = fromInteger $ toInteger mini1
                                 med1   = fromInteger $ toInteger medi1
                                 points = 1.0 - (0.5* (min1**2) / 10000.0  + 0.5 * (med1**2) / 10000.0)
                             in if (points > 0) then points
                                                else 0 

-- for every wikidataID there is one PropsValues data set for it 
-- the result is a sorted list, where the first entry is most likely the name we are searching for
searchTheSuitingWikiDataID:: [WikidataID] -> Name -> [PropsValues] -> Map.Map String Pos -> Map.Map SinglePos String -> [(WikidataID,Name,Points)] 
searchTheSuitingWikiDataID wIDs name pvList stringMap posMap = let zipped            = zip wIDs pvList 
                                                                   widNamePointsList = foldr (\(iD,pvs) acc  -> pointsNameAndMetaData iD name pvs stringMap posMap : acc ) [] zipped 
                                                               in  reverse $ sortOn (\(wikiId,namee,pss) -> pss) widNamePointsList 


                                                               -}