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

-- | getRecords filename loads the file and returns
--   either [Person] or [] depending on loading of 
--   file, It also generates a msg in case of file error
getRecords :: FilePath -> IO [Person]
getRecords fname = do
 content <- safeLoadFile fname 
 case parseStr <$> content of 
  Right records -> return $ validate records 
  _ -> liftIO (print $ "***error:: bad argument provided can't read file named :: "++fname) >> return mempty


-- | converts a Person to Document Type
personToDoc::Person->Document
personToDoc (Person n w)= [(pack "name") := (val n),(pack "weight") := (val w)]

-- | a function which will wrap reading of file in 
--   Either so that we can check if loading of file failed
safeLoadFile :: FilePath -> IO (Either IOException String)
safeLoadFile f = (Right <$> readFile f) `catch` (\ e -> pure (Left e) )

-- | mongoDb function runMongo takes a dbname and a function to run
--   it opens a connection  with given host and runs that function then 
--   closes the connection
runMongo dbName functionToRun = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master (pack dbName) functionToRun
  close pipe

dbName = "PersonDetails"
