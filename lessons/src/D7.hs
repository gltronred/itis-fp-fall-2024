-- | lenses:
-- https://hackage.haskell.org/package/lens

module D7 where

import Data.Function
import Data.Functor.Constant
import Data.Functor.Identity

data FieldB = FieldB
  { _subA :: String
  , _subB :: String
  }
  deriving (Eq,Show,Read)

data Struct = Struct
  { _fieldA :: Int
  , _fieldB :: FieldB
  }
  deriving (Eq,Show,Read)

s1 = Struct
  { _fieldA = 5
  , _fieldB = FieldB
    { _subA = "asdfasdf"
    , _subB = "bcd"
    }
  }

-- s1.fieldA = 1
s2 = s1 { _fieldA = 1 }
-- s1.fieldB.subA = "x"
s3 = s1 { _fieldB = (_fieldB s1) { _subA = "x" } }
s4 = s1 {
  _fieldB = (_fieldB s1) {
      _subA = take 4 (_subA $ _fieldB s1) } }

---------------------------------------------

s5 = s1 {
  _fieldB = (_fieldB s1) {
      _subA = take 4 (s1 & _fieldB & _subA) } }

getFieldB :: Struct -> FieldB
getFieldB = _fieldB

setFieldB :: Struct -> FieldB -> Struct
setFieldB s f = s { _fieldB = f }

modifyFieldB :: (FieldB -> FieldB) -> Struct -> (Struct, FieldB)
modifyFieldB f s = let
  old = _fieldB s
  in (s { _fieldB = f old }, old)

---------------------------------------------

fieldB :: Functor f => (FieldB -> f FieldB) -> (Struct -> f Struct)
fieldB f s = let
  old = _fieldB s  -- FieldB
  new = f old      -- f FieldB
  in (\n -> s { _fieldB = n}) <$> new

setFB :: FieldB -> Identity FieldB
setFB _ = pure $ FieldB { _subA = "ok", _subB = "" }

getFB :: FieldB -> Constant FieldB FieldB
getFB b = Constant b

subA :: Functor f => (String -> f String) -> FieldB -> f FieldB
subA f s = let
  old = _subA s
  new = f old
  in (\n -> s { _subA = n }) <$> new

getSA :: String -> Constant String String
getSA b = Constant b
