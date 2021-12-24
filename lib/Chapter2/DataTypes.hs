module Chapter2.DataTypes where

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data Person = Person String String Gender
  deriving (Show)

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show)

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual (Person first_name last_name _) _ -> first_name ++ " " ++ last_name

companyName :: Client -> Maybe String
companyName client = case client of
  Company name _ _ _ -> Just name
  _ -> Nothing
