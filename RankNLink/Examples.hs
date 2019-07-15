{-# LANGUAGE OverloadedStrings #-} 


module Examples where

import PositionMap
import ComputeProbs
import Json 
import qualified Data.Map as Map 
import Data.Text.Internal
import Data.ByteString.Lazy as L 

{-
exStr1 = " Max Planck was a German theoretical physicist . His name in native language is Max Planck (German). Max Planck was born in Kiel . Franz Ferdinand is also a Person."

numbered1 = numberTheString exStr1

stringTree = initStrKV exStr1

posTree    = initPosKV exStr1

findPlanck = findString "Max Planck" stringTree posTree

planckNPhysicist = distancesBetweenWordNName "Max Planck" "physicist" stringTree posTree

planckNWas = distancesBetweenWordNName "Max Planck" "was" stringTree posTree

wasNPhysicist = distancesBetweenWordsNName "Max Planck" ["physicist","was"] stringTree posTree

propsVs = [("Place of Birth","Kiel"),("Job","physicist"),("Place Of Death","Idk")]

propsVs2 = [("Place of Birth","SampleTown"),("Job","physicist"),("Place Of Death","Idk")]


pointEx1 = pointsNameAndMetaData "sampleID" "Max Planck" propsVs stringTree posTree

pointEx2 = pointsNameAndMetaData "sampleID" "Franz Ferdinand" propsVs2 stringTree posTree

suiting1 = searchTheSuitingWikiDataID ["ex1","ex2"] "Max Planck" [propsVs,propsVs2] stringTree posTree 
-}

exBuild = buildJSON [("name1",[("link123","5.7"),("link22","3.7")]), ("name2",[("link77","6.7"),("link22","2.0")] )]  


wikiP1  = WikiPerson "Max Planck" "sampleLink123" ["moin", "human", "hello"]

exWikiPs = [wikiP1]

person1 = PersonInText "Max Planck" exWikiPs

exPersons = [person1]

exQueryRes = QueryRes "Max Planck is a human" exPersons

exPs = computePoints exQueryRes 

builtV = buildJSON exPs 

builtJSON = jsonValueToJSONString builtV 

toMoin    = L.writeFile "moin.json" builtJSON  
-- smaller

strTr = initStrKV "Max Planck is a human a"

posTr = initPosKV "Max Planck is a human a"

smallest = pointsfor1Word "human" 0 strTr

smaller = findWord "human" strTr

small  = computeOne strTr posTr person1 

 

-- ranking1 = getRanking strTr 