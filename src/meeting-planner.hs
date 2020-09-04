{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Construct a URI for timeanddate.com's international meeting planner.
--

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time as Time
import Text.Printf (printf)

-- World Clock examples
-- https://www.timeanddate.com/worldclock/meetingtime.html?p1=224&p2=24&p3=43&p4=1323&p5=37&p6=438&p7=47&iso=20200228

-- Astronomy examples
--  https://www.timeanddate.com/astronomy/australia/brisbane
-- https://www.timeanddate.com/astronomy/uk/nottingham

-- Meeting Planner examples
-- e.g. "https://www.timeanddate.com/worldclock/meetingtime.html?iso=20200204&p1=43&p2=64&p3=224&p4=438&p5=248&p6=47&p7=22

--base :: Text.Text
base = "https://www.timeanddate.com/worldclock/meetingtime.html"

usa =
  [ 224, -- San Francisco, USA
    24, -- Austin, USA
    43 -- Boston, USA
  ]

eu =
  [ 136, -- London, England
    37 -- Berlin, Germany
  ]

asiaPacific =
  [ 438, -- Bangalore, India
    145, -- Manilla, Phillipines
    248, -- Tokyo, Japan
    22 -- Auckland, New Zealand
  ]

home =
  [ 47 -- Brisbane, Australia
  ]

personal = home ++ usa

cities :: [Int]
cities = personal

main = do
  c <- Time.getCurrentTime
  let (y, m, d) = Time.toGregorian $ Time.utctDay c
  -- Date like yyyymmdd
  let date = T.pack $ printf "%4d%02d%02d" y m d
  T.putStrLn $ uri date
  where
    f i city = T.pack $ printf "p%d=%d" i city
    cs = zipWith f ([1 ..] :: [Int]) cities
    ps d = cs <> ["iso=" <> d]
    uri d = base <> "?" <> T.intercalate "&" (ps d)
