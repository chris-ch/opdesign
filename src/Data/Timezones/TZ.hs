module Data.Timezones.TZ where
    
import Prelude (String, error, read, Bool(..), Eq)
import Prelude ((*), ($))

import Data.String (IsString)
import Data.Time (UTCTime, LocalTime, TimeZone(..), localTimeToUTC)

tzEST :: TimeZone
tzEST = TimeZone (-5 * 60) True "EST"

tzEDT :: TimeZone
tzEDT = TimeZone (-4 * 60) True "EDT"

tzGMT :: TimeZone
tzGMT = TimeZone (0 * 60) True "GMT"

tzUTC :: TimeZone
tzUTC = TimeZone (0 * 60) True "UTC"

tzBST :: TimeZone
tzBST = TimeZone (1 * 60) True "BST"

tzParse :: (Eq a, IsString a) => a -> TimeZone
tzParse text = case text of
    "EST" -> tzEST
    "EDT" -> tzEDT
    "GMT" -> tzGMT
    "UTC" -> tzUTC
    "BST" -> tzBST
    _ -> error "Invalid timezone"

asUTC :: TimeZone -> String -> UTCTime
asUTC tz dateText = localTimeToUTC tz $ asLocalTime dateText

asLocalTime :: String -> LocalTime
asLocalTime dateText = (read dateText :: LocalTime)
