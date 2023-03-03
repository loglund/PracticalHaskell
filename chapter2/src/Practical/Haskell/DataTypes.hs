{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Practical.Haskell.DataTypes where

import Data.Char (toUpper)

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Int
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR}
             deriving Show

data Person = Person String String Gender
  deriving (Show)

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show)

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer

data ConnOptions = ConnOptions { connType :: ConnType
                               , connSpeed :: Integer
                               , connProxy :: UseProxy
                               , connCaching :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut :: TimeOut
                               }


clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual (Person first_name last_name _) _ -> first_name ++ " " ++ last_name

companyName :: Client -> Maybe String
companyName client = case client of
  Company name _ _ _ -> Just name
  _ -> Nothing

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR { .. } = "Hi, " ++ clientRName
greet GovOrgR { } = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
        let newName = (toUpper initial):rest
        in  p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p