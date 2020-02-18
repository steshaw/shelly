--
-- Construct a URI for timeanddate.com's international meeting planner.
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as List
import qualified Data.Time as Time
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- e.g. "https://www.timeanddate.com/worldclock/meetingtime.html?iso=20200204&p1=43&p2=64&p3=224&p4=438&p5=248&p6=47&p7=22

--base :: Text.Text
base = "https://www.timeanddate.com/worldclock/meetingtime.html"

cities :: [Int]
cities=
  [ 224 -- San Francisco
  , 24  -- Austin
  , 43  -- Boston
  , 136 -- London
  , 37  -- Berlin
  , 166 -- Moscow
  , 438 -- Bangalore
  , 33  -- Beijing
  , 248 -- Tokyo
  , 47  -- Brisbane
  , 22  -- Auckland
  ]

main = do
  c <- Time.getCurrentTime
  let (y , m , d ) = Time.toGregorian $ Time.utctDay c
  -- Date like yyyymmdd
  let date = T.pack $ printf "%4d%02d%02d" y m d
  T.putStrLn $ uri date
  where
    f i city = T.pack $ printf "p%d=%d" i city
    cs = zipWith f ([1..] :: [Int]) cities
    ps d = cs <> ["iso=" <> d]
    uri d = base <> "?" <> T.intercalate "&" (ps d)
