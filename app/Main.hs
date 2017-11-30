{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Lib

main :: IO ()
main = do
  let
    myRecordPSRep = toPSRep (Proxy @MyRecord)
    myType = "type MyRecord = " ++ myRecordPSRep
  putStrLn myType
