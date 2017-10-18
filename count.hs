{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

getJSON :: IO B.ByteString
getJSON = B.readFile "./notes.json"

data Reference = Reference
  { content_type :: String
  , uuid :: String
  } deriving (Eq, Show)

instance FromJSON Reference where
  parseJSON =
    withObject "???" $ \v -> Reference <$> v .: "content_type" <*> v .: "uuid"

data Content = Content
  { title :: String
  , text :: String
  , references :: [Reference]
  } deriving (Eq, Show)

instance FromJSON Content where
  parseJSON =
    withObject "???" $ \v ->
      Content <$> v .: "title" <*> v .: "text" <*> v .: "references"

data Item = Item
  { uuid' :: String
  , created_at :: String
  , updated_at :: Maybe String
  , content :: Content
  } deriving (Eq, Show)

instance FromJSON Item where
  parseJSON =
    withObject "???" $ \v ->
      Item <$> v .: "uuid" <*> v .: "created_at" <*> v .: "updated_at" <*>
      v .: "content"

main :: IO ()
main = do
  x <- (eitherDecode <$> getJSON) :: IO (Either String [Item])
  case x of
    Left err -> putStrLn err
    Right ws -> print $ length ws
