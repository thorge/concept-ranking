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
import Data.String.Conversions (cs)

type OriginalName = String 

type WikiName     = String 

type WikiLink     = String 

type LinkWord     = String 


type Points = String 


data QueryRes     = QueryRes String [PersonInText]
 deriving (Show,Eq) 

getTxt :: QueryRes -> String 
getTxt (QueryRes t ps) = t 

getPersons :: QueryRes -> [PersonInText]
getPersons (QueryRes t ps) = ps 

data PersonInText = PersonInText   OriginalName [WikiPerson]
 deriving (Show,Eq) 



getOriginalName :: PersonInText -> OriginalName 
getOriginalName (PersonInText oname wL) = oname 

setOriginalName :: String -> PersonInText -> PersonInText 
setOriginalName str (PersonInText oname wL) = (PersonInText str wL)

getWikiPersons :: PersonInText -> [WikiPerson]
getWikiPersons (PersonInText oname wList) = wList  



data WikiPerson   = WikiPerson WikiName  WikiLink [LinkWord]
 deriving (Show,Eq) 


getWikiName :: WikiPerson -> WikiName 
getWikiName (WikiPerson name link lwords) = name

getWikiLink :: WikiPerson -> WikiLink 
getWikiLink (WikiPerson name link lwords) = link  

getWikiWords :: WikiPerson -> [LinkWord] 
getWikiWords (WikiPerson name link lwords) = lwords    

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
-}

-- type Object = HashMap Text Value

getObject :: Value -> Object 
getObject (Object o) = o 


{-
data QueryRes     = QueryRes String [PersonInText]

data PersonInText = PersonInText  [WikiPerson] OriginalName

data WikiPerson   = WikiPerson [LinkWord] WikiLink WikiName

 

getIt :: IO QueryRes 
getIt = do (Just v) <- try2
           return $ queryToQueryRes v   

-}
readAndBuildValue :: FilePath -> IO (Maybe Value) 
readAndBuildValue filepath = do bStr <- L.readFile filepath 
                                return $ decode bStr 


queryToQueryRes :: Value -> QueryRes
queryToQueryRes obj = let o                                = getObject obj 
                          (Just (String inputText) )       = HM.lookup "text"  o 
                          (Just nameArray)                 = HM.lookup "names" o
                          personList                       = getPersonsInTextFromArray nameArray 
                      in (QueryRes (cs inputText) personList)


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
                     in (PersonInText (cs name) (resultArrayToWikiPersonList resultArray))


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

myFromString :: Value -> String 
myFromString (String t) = cs t 
         



val :: Value
val = Object $ fromList [("numbers", Array $ fromList [Number 1, Number 2, Number 3]), ("boolean", Bool True) ]


  -- build the resultjson 
  -- we need : 
  -- a List of the originalnames in the passsed order and a corresponding list of haspmaps with the wikidataids as keys and the points as values 


buildJSON :: [(OriginalName,[(WikiLink,Points)])] -> Value  
buildJSON info = Object $ fromList (buildL' info 0) where 
   buildL' :: [(OriginalName,[(WikiLink,Points)])] -> Int -> [(Text,Value)]
   buildL' []  _    = []
   buildL' (i:is) n = let oName      = fst i 
                          linkList   = snd i
                          linkListWithValue = map (\(link,pts) -> ( (cs link) ,String (cs pts))) linkList 
                          hmlinkList = Object $ fromList $ linkListWithValue    
                          linkListHash = Object $ fromList [(cs oName,hmlinkList)]
                      in ( (cs $ show n) ,linkListHash) : buildL' is (n+1)


jsonValueToJSONString :: Value -> L.ByteString 
jsonValueToJSONString  = encode     


{-}

   buildHM' []       = []
   buildHM' (h:hs)   = let link   = fst h 
                           points = snd h
                       in (link,points)    -}