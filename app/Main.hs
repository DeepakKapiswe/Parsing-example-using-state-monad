{-# LANGUAGE DuplicateRecordFields,OverloadedStrings,TemplateHaskell, ExtendedDefaultRules, FlexibleContexts,NoMonomorphismRestriction  #-}

module Main where
import Control.Monad
import Control.Monad.State

import Control.Applicative

import SuffixLenses (suffixLenses)
import Lens.Micro   ((^.), (&), (.~), (%~))

import Data.Monoid

import Data.Char
import Text.ParserCombinators.ReadP hiding (get)

data Mystate = Mystate {
               persons::[Person]
             , currName::Maybe Name
             , currWeigth::Maybe Weight
             , remainingInput :: String
             } deriving (Show,Eq)

data Conf = Inter StateName Mystate | Final Mystate deriving (Eq,Show)

data StateName = Init
               | AcceptSpaces1
               | AcceptWeight
               | AcceptSpaces2
               | AcceptKg deriving (Eq,Show,Ord)

data Person = Person {
              name::Name
            , weight::Weight

              } deriving (Show,Eq)

type Name = [Char]
type Weight = Float

suffixLenses ''Mystate
suffixLenses ''Person


parseStr::[Char]->[Person]
parseStr = u

transition::State Conf Conf
transition = do
 conf <- get
 case conf of 
  Final _ -> return conf
  Inter _ st |endOfInput st -> return (Final st)
  _ -> do modify modifyConf
          transition

modifyConf::Conf->Conf
modifyConf c = case c of
 Inter Init st -> case parse parseName (st ^. remainingInputL) of
                   Right (pname,rem) -> Inter AcceptSpaces1 (st & ((remainingInputL .~ rem ).( currNameL %~ (pname <$))))
                   Left _ -> Inter Init (removeHead st)
 Inter AcceptSpaces1 st ->  case parse parseSpaces (st ^. remainingInputL) of 
                   Right (_,rem) -> Inter AcceptWeight (st & (remainingInputL .~ rem))
                   Left _ -> Inter Init (gotoInitState st)


getHead::Mystate->Char
getHead st = head $ st ^. remainingInputL

removeHead::Mystate->Mystate
removeHead =  (%~) remainingInputL (drop 1)

gotoInitState::Mystate->Mystate
gotoInitState st = st & (currNameL .~ Nothing).(currWeigthL .~ Nothing).removeHead

-- | confirms whether all of the input string is consumed
endOfInput::Mystate->Bool
endOfInput st = st ^. remainingInputL == ""


parseName::ReadP String
parseName = many1 (satisfy isLetter)

parseSpaces::ReadP String
parseSpaces = many1 (char ' ')


parse::ReadP a-> String-> Either String (a,String)
parse p s = case readP_to_S p s of 
 [] -> Left s
 xs -> Right (last xs)

u = undefined

is::String->Mystate
is i = Mystate mempty mempty Nothing i

-- | makes initial configuration from a given string
makeInitConf::String->Conf
makeInitConf inp = Inter Init (is inp)

iss = makeInitConf i

i = "this/ si /@3 3432 djw2tony 32kg ewij#hari 75kg$jda"

main :: IO ()
main = print "dfd"
