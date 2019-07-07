module Examples where

import PositionMap


exStr1 = " Max Planck was a German theoretical physicist . His name in native language is Max Planck (German). Max Planck was born in Kiel "

numbered1 = numberTheString exStr1

stringTree = initStrKV exStr1

posTree    = initPosKV exStr1

findPlanck = findString "Max Planck" stringTree posTree

planckNPhysicist = distancesBetweenWordNName "Max Planck" "physicist" stringTree posTree

planckNWas = distancesBetweenWordNName "Max Planck" "was" stringTree posTree

wasNPhysicist = distancesBetweenWordsNName "Max Planck" ["physicist","was"] stringTree posTree

propsVs = [("Place of Birth","Kiel"),("Job","physicist"),("Place Of Death","Idk")]
