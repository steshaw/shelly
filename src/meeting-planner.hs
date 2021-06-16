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
--
-- https://www.timeanddate.com/worldclock/meetingtime.html?p1=224&p2=24&p3=43&p4=1323&p5=37&p6=438&p7=47&iso=20200228
-- San Francisco, Austin, Boston, London, Moscow, Bengaluru, Brisbane, Auckland
-- https://www.timeanddate.com/worldclock/personal.html?cities=224,24,43,136,166,438,47,22&wch=2

-- Astronomy examples
--  https://www.timeanddate.com/astronomy/australia/brisbane
-- https://www.timeanddate.com/astronomy/uk/nottingham

-- Meeting Planner examples
-- e.g. "https://www.timeanddate.com/worldclock/meetingtime.html?iso=20200204&p1=43&p2=64&p3=224&p4=438&p5=248&p6=47&p7=22

--base :: Text.Text
base = "https://www.timeanddate.com/worldclock/meetingtime.html"

sf = 224 -- San Francisco, USA
austin = 24 -- Austin, USA
boston = 43 -- Boston, USA

usa =
  [ sf,
    austin,
    boston
  ]

london = 136 -- London, England
uk = london
berlin = 37 -- Berlin, Germany
moscow = 166
russia = moscow

eu =
  [ uk,
    berlin,
    moscow
  ]

bengaluru = 438 -- Bengaluru, India
india = bengaluru
manilla = 145 -- Manilla, Phillipines
tokyo = 248 -- Tokyo, Japan
auckland = 22 -- Auckland, New Zealand
nz = auckland

asiaPacific =
  [ bengaluru,
    manilla,
    tokyo,
    auckland
  ]

perth = 196 -- Perth, Australia
brisbane = 47 -- Brisbane, Australia
melbourne = 152 -- Melbourne, Australia

au =
  [ perth
  , brisbane
  , melbourne
  ]

home =
  [ brisbane
  ]

mlabs = [sf, austin, boston, uk, russia, bengaluru, brisbane, nz]

cities :: [Int]
cities = mlabs

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
