{-# LANGUAGE OverloadedStrings #-} 


import qualified Data.ByteString.Lazy as L

import Json 
import ComputeProbs 
import Data.Maybe(fromJust)
import System.Environment(getArgs)

-- cabal install string-conversions 

type FilePathInput = FilePath 

type FilePathOutPut = FilePath 

main = do
         args <- getArgs
         case args of 
          [ins,outs] -> compute ins outs 
          _          -> putStrLn "wrong nmber of arguments"


compute :: FilePathInput -> FilePathOutPut -> IO ()
compute  inputPath outPutPath =  do maybeV <- readAndBuildValue inputPath 
                                    case maybeV of 
                                                   Nothing  -> error "can not read the given jsonfile"
                                                   (Just v) ->  return ()
                                    let readV        = fromJust maybeV
                                    let queryRes     = queryToQueryRes readV
                                    let computation  = computePoints  queryRes 
                                    let resJSONValue = buildJSON computation 
                                    let resJSON      =  jsonValueToJSONString resJSONValue 
                                    --putStrLn $ show readV
                                    putStrLn $ show queryRes 
                                    -- putStrLn $ show computation 
                                    L.writeFile outPutPath resJSON
                                    return ()





