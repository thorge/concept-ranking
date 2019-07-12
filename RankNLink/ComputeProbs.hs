module ComputeProbs where 


import PositionMap
import SmallFunctions 
import qualified Data.Map as Map

import Data.List(sort,sortOn)
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

type Points = Float 



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