module Description where

import Data.Char (isSpace)
import Text.Printf

data Item f = Item
  { comment :: String,
    formula :: f
  }
  deriving (Show)

instance Functor Item where
  fmap f i = i {formula = f (formula i)}

data Description f = Description
  { items :: [Item f],
    name :: String
  }
  deriving (Show)

type Result = Description Int

parseItems :: [String] -> Maybe [Item String]
parseItems [] = Just []
parseItems [_] = Nothing
parseItems (c : f : rest) = (Item c f :) <$> parseItems rest

parse :: String -> Maybe (Description String)
parse input = case filter ((/= ';') . head) . filter (not . null) . map (dropWhile isSpace) $ lines input of
  [] -> Nothing
  n : remainder -> do
    is <- parseItems remainder
    pure $ Description is n

outOf :: Int -> Int -> String
outOf r t = printf "%.2f%%" (100 * fromIntegral r / fromIntegral t :: Double)

pprint :: Int -> Result -> String
pprint nrRuns desc =
  unlines
    [ name desc,
      "",
      "there are a total of " ++ show nrRuns ++ " runs in this test:"
    ]
    ++ unlines
      [ "  " ++ comment i ++ ": " ++ show f ++ " (" ++ f `outOf` nrRuns ++ ")"
        | i <- items desc,
          let f = formula i
      ]
