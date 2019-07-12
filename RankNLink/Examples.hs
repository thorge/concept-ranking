module Examples where

import PositionMap
import ComputeProbs

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