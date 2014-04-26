
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Named

data TBanana = TBanana
  { tshape :: Field "banana-shape" Text
  , tsize  :: Field "banana size" (Maybe Int)
  , tname  :: Field "banana's name" Text
  } deriving Show

deriveToJSONFields ''TBanana

b = Banana "foo" (Just 2) "bar"
