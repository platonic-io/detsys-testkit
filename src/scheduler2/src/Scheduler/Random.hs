module Scheduler.Random where

type Seed = Integer

data Capability m = Capability
  { gen :: Int -> m [Int]
  }
