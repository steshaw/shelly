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

sf = 224 -- San Francisco, USA
austin = 24 -- Austin, USA
boston = 43 -- Boston, USA

usa =
  [ sf
  , austin
  , boston
  ]

toronto = 250 -- Toronto, Ontario, Canada

london = 136 -- London, England
uk = london
paris = 195
berlin = 37 -- Berlin, Germany
moscow = 166
russia = moscow
warsaw = 262

-- https://www.timeanddate.com/worldclock/meetingtime.html?iso=20210727&p1=250&p2=136&p3=262&p4=166&p5=47

eu =
  [ uk
  , paris
  , moscow
  ]

bengaluru = 438 -- Bengaluru, India
india = bengaluru
manilla = 145 -- Manilla, Phillipines
tokyo = 248 -- Tokyo, Japan
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

cities :: [Int]
cities = [sf, austin, boston, london, paris, india, perth, brisbane]

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
