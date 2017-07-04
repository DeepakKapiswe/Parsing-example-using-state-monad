{-# LANGUAGE TemplateHaskell #-}

module MyTypes where
import SuffixLenses (suffixLenses)

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
