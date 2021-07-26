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
import Text.Printf (printf)
import System.Environment as Sys
import System.Exit as Sys

-- Astronomy examples
--   https://www.timeanddate.com/astronomy/australia/brisbane
--   https://www.timeanddate.com/astronomy/uk/nottingham

sf = 224 -- San Francisco, USA
austin = 24 -- Austin, USA
boston = 43 -- Boston, USA

usa =
  [ sf,
    austin,
    boston
  ]

toronto = 250 -- Toronto, Ontario, Canada

london = 136 -- London, England
uk = london
berlin = 37 -- Berlin, Germany
moscow = 166
russia = moscow
warsaw = 262
-- https://www.timeanddate.com/worldclock/meetingtime.html?iso=20210727&p1=250&p2=136&p3=262&p4=166&p5=47

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

mlabs = [sf, austin, toronto, uk, russia, bengaluru, brisbane, nz]
team = [toronto, uk, warsaw, russia, brisbane]

cities :: [Int]
cities = team

cs = zipWith f ([1 ..] :: [Int]) cities
 where
  f i city = T.pack $ printf "p%d=%d" i city

--
-- For example:
--   San Francisco, Austin, Boston, London, Moscow, Bengaluru, Brisbane, Auckland
--   https://www.timeanddate.com/worldclock/meetingtime.html?cities=224,24,43,136,166,438,47,22&wch=2
--
time = do
  c <- Time.getCurrentTime
  let (y, m, d) = Time.toGregorian $ Time.utctDay c
  -- Date like yyyymmdd
  let date = T.pack $ printf "%4d%02d%02d" y m d
  T.putStrLn $ uri date
  where
    ps d = cs <> ["iso=" <> d]
    uri d = base <> "?" <> T.intercalate "&" (ps d)
    base = "https://www.timeanddate.com/worldclock/meetingtime.html"

--
-- For example:
--   San Francisco, Austin, Boston, London, Moscow, Bengaluru, Brisbane, Auckland
--   https://www.timeanddate.com/worldclock/personal.html?cities=224,24,43,136,166,438,47,22&wch=2
--
clock = do
  T.putStrLn uri
 where
  uri = base <> "?" <> T.intercalate "&" cs
  base = "https://www.timeanddate.com/worldclock/personal.html"

main = do
  args <- Sys.getArgs
  case args of
    ["clock"] -> clock
    ["time"] -> time
    [] -> time
    _ -> do
      putStrLn "usage: meeting [time|clock]"
      Sys.exitFailure
