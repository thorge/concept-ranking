{-# LANGUAGE OverloadedStrings #-} 


import qualified Data.ByteString.Lazy as L

import Json 
import ComputeProbs 


type FilePathInput = FilePath 

type FilePathOutPut = FilePath 

main = compute "in" "out"


compute :: FilePathInput -> FilePathOutPut -> IO ()
compute  inputPath outPutPath = undefined  



