module Examples where 

import Position


exStr1 = " Max Planck was a German theoretical physicist . His name in native language is Max Planck (German). Max Planck was born in Kiel "

numbered1 = numberTheString exStr1 

stringTree = initStringTree exStr1 

posTree    = initPosTree exStr1

findPlanck = findString "Max Planck" stringTree posTree 

planckNPhysicist = distancesBetweenWordNName "Max Planck" "physicist" stringTree posTree 

planckNWas = distancesBetweenWordNName "Max Planck" "was" stringTree posTree 