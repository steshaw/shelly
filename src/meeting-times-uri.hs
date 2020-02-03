--
-- Construct a URI for timeanddate.com's international meeting planner.
--

import Data.List as List

-- e.g. "https://www.timeanddate.com/worldclock/meetingtime.html?iso=20200204&p1=43&p2=64&p3=224&p4=438&p5=248&p6=47&p7=22

base="https://www.timeanddate.com/worldclock/meetingtime.html"
date="iso=20200204"

cities=
  [ 43  -- Boston
  , 64  -- Chicago
  , 224 -- San Francisco
  , 136 -- London
  , 438 -- Bangalore
  , 248 -- Tokyo
  , 47  -- Brisbane
  , 22  -- Auckland
  ]

main = do
  putStrLn uri
  where
    f (i, city) = "p" <> show i <> "=" <> show city
    cs = map f $ zip [1..] cities
    uri = base <> "?" <> date <> "&" <> List.intercalate "&" cs
