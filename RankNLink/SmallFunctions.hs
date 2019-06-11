module SmallFunctions where 



-- gets the submedian of a generic ordered list
median :: [a] -> a 
median [] = error "can not get a median from an empty list"
median as = let lengthL = length as 
                med_pos = (lengthL - 1) `div` 2
            in as !! med_pos 

--returns the median and the position of the median as a tupel 
medianWithPos :: [a] -> (a,Int) 
medianWithPos  [] = error "can not get a median from an empty list"
medianWithPos  as = let lengthL = length as 
                        med_pos = (lengthL - 1) `div` 2
                        med     = as !! med_pos
                in  (med,med_pos )

-----------------------------------------------------------------

average :: [Int] -> Float 
average list = let (sumL,lengthL)  = foldr (\i (s,l) -> ( (s+i) , (l+1) ) ) (0,0)  mapped 
                   mapped          = map fromIntegral list
               in sumL/lengthL
                   