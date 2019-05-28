module Rankings where 

import PositionMap 


import qualified Data.Map as Map



-- the prob type is a float number which represants percentage
data Prob = Prob Float 


{-

In the module PositionMap we defined some functions with which we can find names,strings,etc. in a given inputstring and compute the distances between those words
within the text.
Now we want to compute probabilities that a certain name is linked with a person name in our database we use (wikidata).






 -}

 -- We need for every related string the names it could be associated with and the found distances to it 
 -- a suiting data structure would be a keymap of type Map RelatedString [(Name,Pos)]