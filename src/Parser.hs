{-# LANGUAGE TemplateHaskell #-}

module Parser where
import Control.Monad
import Control.Monad.State

import Control.Applicative hiding (many)
import MyTypes

import Lens.Micro   ((^.), (&), (.~), (%~))

import Data.Maybe

import Data.Char
import Text.ParserCombinators.ReadP hiding (get)

-- | The top level function which will parse a given
--   string to give list of Person
parseStr::[Char]->[Person]
parseStr = getPersons.evalState transition.makeInitConf

-- | The state monad to parse the Configuration whole transition takes place here
transition::State Conf Conf
transition = do
 conf <- get
 case conf of
  Final _ -> return conf
  Inter _ st |endOfInput st -> return (Final st)
  _ -> do modify modifyConf
          transition

-- | A function to modify the Configuration according to the Configuration and input string
--   this function contains the main logic of DFA for changing the state or Configuration
modifyConf::Conf->Conf
modifyConf c = case c of
 Inter Init st -> case parse parseName (st ^. remainingInputL) of
                   Right (pname,rem) -> Inter AcceptSpaces1 $ st & 
                                        remainingInputL .~ rem   &
                                        currNameL .~ pure pname
                   Left _ -> Inter Init . removeHead $ st

 Inter AcceptSpaces1 st ->  case parse parseSpaces (st ^. remainingInputL) of
                   Right (_,rem) -> Inter AcceptWeight $ st &
                                    remainingInputL .~ rem
                   Left _ -> Inter Init . gotoInitState $ st

 Inter AcceptWeight st -> case parse parseFloat (st ^. remainingInputL) of
                   Right (pweight,rem) -> Inter AcceptKg $
                             st & remainingInputL .~ rem & 
                             currWeigthL .~ pure pweight
                   Left _ -> Inter Init . gotoInitState $ st

 Inter AcceptKg st -> case parse parseKg (st ^. remainingInputL) of
                   Right (pkg,rem)-> let cn = fromJust $ st ^. currNameL
                                         cw = fromJust $ st ^. currWeigthL
                                     in case rem of
                                        [] -> Final $ st & (personsL %~ ((Person cn cw):))
                                        (x:rest) |not (isLetter x) -> Inter Init . gotoInitState $ st &
                                                                      (remainingInputL .~ rest).
                                                                      (personsL %~ ((Person cn cw):))
                                        _->Inter Init .gotoInitState $ st
                   Left _ -> Inter Init . gotoInitState $ st

-- | gives the head character of the remainingInput of a given Mystate
getHead::Mystate->Char
getHead st = head $ st ^. remainingInputL

-- | drops or consumes the head of the remainingInput of a given Mystate
removeHead::Mystate->Mystate
removeHead =  (%~) remainingInputL (drop 1)

-- | sets the currName and currWeigth field to Nothing of a given Mystate 
gotoInitState::Mystate->Mystate
gotoInitState st = st & (currNameL .~ Nothing).(currWeigthL .~ Nothing)

-- | confirms whether all of the input string is consumed
endOfInput::Mystate->Bool
endOfInput st = st ^. remainingInputL == ""

-- | A parser for name i.e only alphabets are accepted
parseName::ReadP String
parseName = many1 (satisfy isLetter)

-- | parses one or more spaces
parseSpaces::ReadP String
parseSpaces = many1 (char ' ')

-- | A float parser
parseFloat::ReadP Float
parseFloat = fmap read $ (++) <$> number <*> decimal
 where
  decimal = option "" $ (:) <$> char '.' <*> number
  number = many1 (satisfy isDigit)

-- | parses zero or more spaces followed by "kg" keyword
parseKg::ReadP String
parseKg = do
     many (char ' ')
     string "kg"

-- | A function to wrap the output of parser with Either
parse::ReadP a-> String-> Either String (a,String)
parse p s = case readP_to_S p s of
 [] -> Left s
 xs -> Right (last xs)

-- | gives list of persons from a given Conf if 
--   its (Final something) else emptylist
getPersons::Conf->[Person]
getPersons (Final st) = st ^. personsL 
getPersons _ = []


-- | makes initial configuration from a given string
makeInitConf::String->Conf
makeInitConf = Inter Init . Mystate mempty mempty Nothing 

-- | validates a Person according to given conditions of 
--   minimum and maximum weights
validate::[Person]->[Person]
validate = filter (\(Person _ w) -> w >= minWeight && w<= maxWeight)

minWeight = 0.02
maxWeight = 300

