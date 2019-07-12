{-# LANGUAGE OverloadedStrings #-} 

module Json where 

import Data.Aeson 
import qualified Data.ByteString.Lazy as L 

{-Examples  

:set -XOverloadedStrings

encode [1,2,3]
"[1,2,3]"


decode "[1,2,3]" :: Maybe [Integer]
Just [1,2,3]


If you want to see the error too, use eitherDecode:

> eitherDecode "[]" :: Either String Integer
Left "Error in $: expected Integral, encountered Array"




 -}
try :: IO (Maybe Value)
try = do f <- L.readFile "result.json"
         return (decode f :: Maybe Value)