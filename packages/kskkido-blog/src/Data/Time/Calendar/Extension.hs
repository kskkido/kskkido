module Data.Time.Calendar.Extension where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified Data.Time.Calendar as Time.Calendar

toFirstDayOfMonth :: Time.Calendar.Day -> Time.Calendar.Day
toFirstDayOfMonth day =
  let (year, month, _) = Time.Calendar.toGregorian day
   in Time.Calendar.fromGregorian year month 1

toMonthText :: Time.Calendar.Day -> Text
toMonthText day =
  let (year, month, _) = Time.Calendar.toGregorian day
   in Text.pack $ show year <> "-" <> show month

toText :: Time.Calendar.Day -> Text
toText day =
  let (year, month, date) = Time.Calendar.toGregorian day
   in
     ( ( [show year, show month, show date] ) &
       ( Text.pack . List.intercalate "-" )
     )

toYear :: Time.Calendar.Day -> Integer
toYear day =
  let (year, _, _) = Time.Calendar.toGregorian day
   in year
