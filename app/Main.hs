{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import MyTypes
import Parser

import Database.MongoDB hiding (modify)
import Data.Text (pack)

import Control.Monad.Trans (liftIO)
import System.Environment (getArgs)

import Control.Exception


main :: IO ()
main = do
 args <-  getArgs
 case args of 
  []-> print "**exception::no argument povided atleat one required"
  xs -> mapM_ processArg xs

processArg::FilePath->IO ()
processArg arg = do
 recs <-fmap (personToDoc <$>) .getRecords $ arg
 runMongo dbName $ insertMany "persons" recs

getRecords fname = do
 content <- safeLoadFile fname 
 case parseStr <$> content of 
  Right records -> return $ validate records 
  _ -> liftIO (print $ "***error:: bad argument provided can't read file named :: "++fname) >> return mempty

personToDoc::Person->Document
personToDoc (Person n w)= [(pack "name") := (val n),(pack "weight") := (val w)]

safeLoadFile :: FilePath -> IO (Either IOException String)
safeLoadFile f = (Right <$> readFile f) `catch` (\ e -> pure (Left e) )

runMongo dbName functionToRun = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master (pack dbName) functionToRun
  close pipe

dbName = "PersonDetails"
