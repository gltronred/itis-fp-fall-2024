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
--   | Error
--     { errorText :: String }
  deriving (Eq,Show,Read,Generic)

deriving via Generically Message instance ToJSON Message

instance FromJSON Message

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
