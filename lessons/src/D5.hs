-- |

{-# LANGUAGE OverloadedStrings #-}
module D5 where

import Data.Aeson
import Data.Aeson.Types

data JSON
  = Obj [(String, JSON)]
  | Arr [JSON]
  | Str String
  | Num Double
  | Bool Bool
  | Null

data Message
  = Command
    { commandType :: String
    , commandArgs :: [Int]
    }
--   | Error
--     { errorText :: String }
  deriving (Eq,Show,Read)

{-

{
   "commandType": "add",
   "commandArgs": [3,4,5]
}

хочется парсить во что-то такое:
Command "add" [3,4,5]

чем во что-то такое:
Object [("commandType", String "add"), ("commandArgs", Array [Number 3, Number 4, Number 5])]

-}

-- Class and Instance

instance FromJSON Message where
  parseJSON (Object v) = Command
    <$> v .: "commandType"
    <*> v .: "commandArgs"
  parseJSON invalid = typeMismatch "Message" invalid

instance ToJSON Message where
  toJSON (Command x y) = error "Implement me!"
  toEncoding (Command x y) = error "Implement me!"
