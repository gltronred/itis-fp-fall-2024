-- |

module D5 where

import Data.Aeson

data JSON
  = Object [(String, JSON)]
  | Array [JSON]
  | String String
  | Number Double
  | Bool Bool
  | Null

data Message
  = Command
    { commandType :: String
    , commandArgs :: [Int]
    }
--   | Error
--     { errorText :: String }

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
