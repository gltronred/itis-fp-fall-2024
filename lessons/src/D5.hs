-- |

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module D5 where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

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
  | Error
    { errorText :: String }
  deriving (Eq,Show,Read,Generic)

customOptions = defaultOptions { sumEncoding = defaultTaggedObject }

instance FromJSON Message where
  parseJSON = genericParseJSON customOptions
instance ToJSON Message where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

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

-- Class and Instance (manual)

-- instance FromJSON Message where
--   parseJSON (Object v) = Command
--     <$> v .: "commandType"
--     <*> v .: "commandArgs"
--   parseJSON invalid = typeMismatch "Message" invalid
--
-- instance ToJSON Message where
--   toJSON (Command typ args) = object [ "commandType" .= typ, "commandArgs" .= args ]
--   toEncoding (Command typ args) = pairs ( "commandType" .= typ <> "commandArgs" .= args )

{-
Хотим декодировать строку как время вместо String:

newtype Time = Time String

Пишем новый экземпляр для Time со своими правилами, получаем:

"2024-12-07 10:54:55" --> ... :: Time
"asdfasdfa" --> не парсится как Time
-}

{-
Алгебраические типы

data JSON
  = Obj [(String, JSON)]
  | Arr [JSON]
  | Str String
  | Num Double
  | Bool Bool
  | Null

Сумма типов: Obj + Arr + Str + Num + Bool + Null

data Message
  = Command
    { commandType :: String
    , commandArgs :: [Int]
    }

Произведение типов: String * [Int]
-}

{-
Варианты кодирования типов-сумм (SumEncoding):

TwoElemArray:
ghci> encode $ D5.Error "adfasdf"
"[\"Error\",{\"errorText\":\"adfasdf\"}]"
ghci> encode $ Command "add" [1,2,3]
"[\"Command\",{\"commandType\":\"add\",\"commandArgs\":[1,2,3]}]"

ObjectWithSingleField:
ghci> encode $ D5.Error "adfasdf"
"{\"Error\":{\"errorText\":\"adfasdf\"}}"
ghci> encode $ Command "add" [1,2,3]
"{\"Command\":{\"commandType\":\"add\",\"commandArgs\":[1,2,3]}}"

UntaggedValue:
ghci> encode $ D5.Error "adfasdf"
"{\"errorText\":\"adfasdf\"}"
ghci> encode $ Command "add" [1,2,3]
"{\"commandType\":\"add\",\"commandArgs\":[1,2,3]}"

TaggedObject:
ghci> encode $ D5.Error "adfasdf"
"{\"tag\":\"Error\",\"errorText\":\"adfasdf\"}"
ghci> encode $ Command "add" [1,2,3]
"{\"tag\":\"Command\",\"commandType\":\"add\",\"commandArgs\":[1,2,3]}"

-}
