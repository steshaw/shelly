{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Construct a URI for timeanddate.com's international meeting planner and
-- world clock.
--

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time as Time
import System.Environment as Sys
import System.Exit as Sys
import Text.Printf (printf)

-- Astronomy examples
--   https://www.timeanddate.com/astronomy/australia/brisbane
--   https://www.timeanddate.com/astronomy/uk/nottingham

-- Americas

-- USA
sf = 224 -- San Francisco, CA
la = 137 -- Los Angeles, CA
austin = 24 -- Austin, TX
chicago = 64 -- Chicago, IL
boston = 43 -- Boston, MA
reston = 3810 -- Reston, VA

usa =
  [ sf
  , austin
  , boston
  ]

-- Canada
toronto = 250 -- Toronto, Ontario, Canada
canada = toronto

-- South America
montevideo = 163
uruguay = montevideo

-- Europe

london = 136 -- London, England
uk = london
paris = 195
france = paris
berlin = 37 -- Berlin, Germany
germany = berlin
moscow = 166
russia = moscow
warsaw = 262
poland = warsaw

eu =
  [ uk
  , paris
  , moscow
  ]

-- Asia/Pacific
bengaluru = 438 -- Bengaluru, India
india = bengaluru
pune = 1038 -- Pune, Maharashtra,India
manilla = 145 -- Manilla, Phillipines
phillipines = manilla
tokyo = 248 -- Tokyo, Japan
japan = tokyo
auckland = 22 -- Auckland, New Zealand
nz = auckland

asiaPacific =
  [ bengaluru
  , manilla
  , tokyo
  , auckland
  ]

brisbane = 47 -- Brisbane, Australia
melbourne = 152 -- Melbourne, Australia
perth = 196 -- Perth, Australia
sydney = 240 -- Sydney, Australia

au =
  [ brisbane
  , melbourne
  , sydney
  , perth
  ]

home =
  [ brisbane
  ]

freetime = [london, sf]

work =
  [ la
  , chicago
  , reston
  , montevideo
  , london
  , paris
  , pune
  , perth
  , brisbane
  ]

cities :: [Int]
cities = work

--
-- For example:
--   San Francisco, London, Paris, Brisbane
--   https://www.timeanddate.com/worldclock/meetingtime.html?p1=224&p2=136&p3=195&p4=47&iso=20220206
--
time = do
  c <- Time.getCurrentTime
  let (y, m, d) = Time.toGregorian $ Time.utctDay c
  -- Date like yyyymmdd
  let date = T.pack $ printf "%4d%02d%02d" y m d
  T.putStrLn $ uri date
  where
    cs = zipWith f ([1 ..] :: [Int]) cities
    f i city = T.pack $ printf "p%d=%d" i city
    ps d = cs <> ["iso=" <> d]
    uri d = base <> "?" <> T.intercalate "&" (ps d)
    base = "https://www.timeanddate.com/worldclock/meetingtime.html"

--
-- For example:
--   San Francisco, London, Paris, Brisbane
--   https://www.timeanddate.com/worldclock/personal.html?cities=224,136,195,47
--
clock = do
  T.putStrLn uri
  where
    uri = base <> "?" <> "cities=" <> cs
    base = "https://www.timeanddate.com/worldclock/personal.html"
    cs = T.intercalate "," (map tshow cities)
    tshow = T.pack . show

main = do
  args <- Sys.getArgs
  case args of
    ["clock"] -> clock
    ["time"] -> time
    [] -> time
    _ -> do
      putStrLn "usage: meeting [time|clock]"
      Sys.exitFailure
