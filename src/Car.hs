module Car where
import Data.Aeson
import Data.Text (pack, unpack)

data Car = Car { year           :: Integer
               , make           :: String
               , model          :: String
               , style          :: String
               , color          :: String
               , mileage        :: String
               , airConditioner :: Bool
               , vinNumber      :: String
               , price          :: Double
               , previousOwner  :: String
               } deriving Show

instance ToJSON Car where
  toJSON (Car year make model style color mileage airConditioner vinNumber price previousOwner) =
    object [(pack "year"          ) .= year,
            (pack "make"          ) .= make,
            (pack "model"         ) .= model,
            (pack "style"         ) .= style,
            (pack "color"         ) .= color,
            (pack "mileage"       ) .= mileage,
            (pack "airConditioner") .= airConditioner,
            (pack "vinNumber"     ) .= vinNumber,
            (pack "price"         ) .= price,
            (pack "previousOwner" ) .= previousOwner]
