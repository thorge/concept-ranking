{-# LANGUAGE OverloadedStrings #-} 

module Json where 

import Data.Aeson 
import qualified Data.ByteString.Lazy as L 
import qualified Data.HashMap.Strict as HM
import GHC.Exts 
import Data.Aeson.Types
import Data.Maybe(fromJust )
import Data.Text.Internal
import qualified Data.Vector as V   

type OriginalName = Text 

type WikiName     = Text 

type WikiLink     = Text 

type LinkWord     = Text 


data QueryRes     = QueryRes Text [PersonInText]
 deriving Show 
data PersonInText = PersonInText   OriginalName [WikiPerson]
 deriving Show 

data WikiPerson   = WikiPerson WikiName  WikiLink [LinkWord]
 deriving Show 

{-}
instance FromJSON QueryRes where 
  parseJSON = withObject "QueryRes" $ (\o -> QueryRes <$> o .: "text" <*> o .: "names")

instance FromJSON WikiPerson where 
  parseJSON = withObject "WikiPerson" $ (\o -> WikiPerson <$> o .: "value" <*> o .: "item" <*> o .: "label")

instance FromJSON PersonInText where 
  parseJSON = withObject "PersonInText" $ (\o -> PersonInText <$> o.: "results" <*> o.: "original" )
  -}

{-Examples  

:set -XOverloadedStrings

encode [1,2,3]
"[1,2,3]"


decode "[1,2,3]" :: Maybe [Integer]
Just [1,2,3]


If you want to see the error too, use eitherDecode:

> eitherDecode "[]" :: Either String Integer
Left "Error in $: expected Integral, encountered Array"




Just (Object (fromList [("text",String "The persons Max Planck and Oliver Auge are professors."),
                        ("names",
                          Array [Object (fromList [("results",Array [Object (fromList [("value",Array [String "German",String "regional",String "historian",String "medievalist",String "G\246ppingen",String "male",String "university",String "teacher",String "University",String "Kiel"]),("item",String "http://www.wikidata.org/entity/Q2020047"),("label",String "Oliver Auge")])]),("original",String "Oliver Auge")]),Object (fromList [("results",Array [Object (fromList [("value",Array [String "German",String "theoretical",String "physicist",String "Kiel",String "male",String "G\246ttingen",String "university",String "teacher",String "Ludwig",String "Maximilian",String "University",String "Munich",String "Humboldt",String "Berlin",String "Frederick",String "William",String "Royal",String "Society",String "Russian",String "Academy",String "Sciences",String "Swedish",String "Netherlands",String "Arts",String "Hungarian",String "Q292692",String "Prussian",String "Lincean",String "Saxon",String "Notgemeinschaft",String "der",String "Deutschen",String "Wissenschaft",String "Q2301163",String "Turin",String "Saint",String "Petersburg",String "American",String "Philosophical",String "USSR",String "Kaiser",String "Wilhelm",String "Leopoldina",String "Bavarian",String "Humanities",String "Pontifical",String "Deutsche",String "Physikalische",String "Gesellschaft"]),("item",String "http://www.wikidata.org/entity/Q9021"),("label",String "Max Planck")]),Object (fromList [("value",Array [String "German",String "classical",String "philologist",String "historian",String "high",String "school",String "teacher",String "Feuerbach",String "male",String "Stuttgart",String "pedagogue"]),("item",String "http://www.wikidata.org/entity/Q1913219"),("label",String "Max Planck")])]),("original",String "Max Planck")])])]))



 -}

-- x = Array [Object (fromList [("results",Array [Object (fromList [("value",Array [String "German",String "regional",String "historian",String "medievalist",String "G\246ppingen",String "male",String "university",String "teacher",String "University",String "Kiel"]),("item",String "http://www.wikidata.org/entity/Q2020047"),("label",String "Oliver Auge")])]),("original",String "Oliver Auge")]),Object (fromList [("results",Array [Object (fromList [("value",Array [String "German",String "theoretical",String "physicist",String "Kiel",String "male",String "G\246ttingen",String "university",String "teacher",String "Ludwig",String "Maximilian",String "University",String "Munich",String "Humboldt",String "Berlin",String "Frederick",String "William",String "Royal",String "Society",String "Russian",String "Academy",String "Sciences",String "Swedish",String "Netherlands",String "Arts",String "Hungarian",String "Q292692",String "Prussian",String "Lincean",String "Saxon",String "Notgemeinschaft",String "der",String "Deutschen",String "Wissenschaft",String "Q2301163",String "Turin",String "Saint",String "Petersburg",String "American",String "Philosophical",String "USSR",String "Kaiser",String "Wilhelm",String "Leopoldina",String "Bavarian",String "Humanities",String "Pontifical",String "Deutsche",String "Physikalische",String "Gesellschaft"]),("item",String "http://www.wikidata.org/entity/Q9021"),("label",String "Max Planck")]),Object (fromList [("value",Array [String "German",String "classical",String "philologist",String "historian",String "high",String "school",String "teacher",String "Feuerbach",String "male",String "Stuttgart",String "pedagogue"]),("item",String "http://www.wikidata.org/entity/Q1913219"),("label",String "Max Planck")])]),("original",String "Max Planck")])]



-- x = Object (fromList [("value",Array [String "German",String "regional",String "historian",String "medievalist",String "G\246ppingen",String "male",String "university",String "teacher",String "University",String "Kiel"]),("item",String "http://www.wikidata.org/entity/Q2020047"),("label",String "Oliver Auge")])]),("original",String "Oliver Auge")]
{-try :: IO (Parser QueryRes )
try = do f        <- L.readFile "result.json"
         let withJust =   (decode f :: Maybe Value)
         let v = fromJust withJust
         return ( (parseJSON :: Value -> Parser QueryRes)  v) 

-}
try2 :: IO (Maybe Value  )
try2 = do f        <- L.readFile "result.json"
          return (decode f :: Maybe Value)


try3 :: IO Object 
try3 = do f        <- L.readFile "result.json"
          let  (Just v) = (decode f :: Maybe Value)
          return (getObject v )

try4 :: IO [(Data.Text.Internal.Text, Value)]
try4 = do f        <- L.readFile "result.json"
          let  (Just v) = (decode f :: Maybe Value)
          let o = (getObject v )
          return (HM.toList o)

try5 :: Int ->IO (Data.Text.Internal.Text, Value)
try5 i = do f        <- L.readFile "result.json"
            let  (Just v) = (decode f :: Maybe Value)
            let o = (getObject v )
            return $ (HM.toList o) !! i 


-- type Object = HashMap Text Value

getObject :: Value -> Object 
getObject (Object o) = o 


{-
data QueryRes     = QueryRes String [PersonInText]

data PersonInText = PersonInText  [WikiPerson] OriginalName

data WikiPerson   = WikiPerson [LinkWord] WikiLink WikiName

 -}

getIt :: IO QueryRes 
getIt = do (Just v) <- try2
           return $ queryToQueryRes v   

queryToQueryRes :: Value -> QueryRes
queryToQueryRes obj = let o                                = getObject obj 
                          (Just (String inputText) )       = HM.lookup "text"  o 
                          (Just nameArray)                 = HM.lookup "names" o
                          personList                       = getPersonsInTextFromArray nameArray 
                      in (QueryRes inputText personList)


{-
Object (fromList [("results",Array [Object (fromList [("value",
                     Array [String "German",String "regional",String "historian",String "medievalist",String "G\246ppingen",String "male",String "university",String "teacher",String "University",String "Kiel"]),("item",String "http://www.wikidata.org/entity/Q2020047"),("label",String "Oliver Auge")])]),
                   ("original",String "Oliver Auge")])
 -}
getPersonsInTextFromArray :: Value -> [PersonInText]
getPersonsInTextFromArray (Array a)    = elemsToPIT (V.toList a) where 
        elemsToPIT []                  = []
        elemsToPIT ((Object hm) : os)  = (getPersonInText hm) : elemsToPIT os   

getPersonInText :: HM.HashMap Text Value -> PersonInText 
getPersonInText hm = let (Just (String name)) = HM.lookup "original" hm 
                         (Just resultArray)   = HM.lookup "results"  hm
                     in (PersonInText name (resultArrayToWikiPersonList resultArray))


resultArrayToWikiPersonList ::  Value -> [WikiPerson] 
resultArrayToWikiPersonList (Array a) = let elems = V.toList a
                                        in map getWikiPersonFromHashmapObject elems 

getWikiPersonFromHashmapObject :: Value -> WikiPerson 
getWikiPersonFromHashmapObject (Object hm) = let label  = myFromString $ fromJust $  HM.lookup "label" hm  
                                                 item   = myFromString $ fromJust  $  HM.lookup "item" hm 
                                                 (Array valueArray) = fromJust $  HM.lookup "value" hm
                                                 valueList  = V.toList valueArray
                                                 textList   = map myFromString valueList 
                                             in (WikiPerson label item textList) 

myFromString :: Value -> Text 
myFromString (String t) = t 
         



val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]