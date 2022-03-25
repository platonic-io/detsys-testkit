module Debugger.SequenceDia where

type Name = String

hLine :: Char
hLine = '─'

vLine :: Char
vLine = '│'

mkBoxLines :: Bool -> [(Int, Name)] -> [String]
mkBoxLines isTop allBoxes = let (x,y,z) = go allBoxes in [x,y,z]
  where
  emptySpace i = (replicate i ' ', replicate i ' ', replicate i ' ')
  go :: [(Int, Name)] -> (String, String, String)
  go [] = mempty
  go ((pre,name):xs) =
    emptySpace pre <>
    ("╭" ++ replicate (length name `div` 2 + 1) hLine ++
      (if isTop then [hLine] else "┴") ++ replicate (length name `div` 2 + 1) hLine ++ "╮"
    , [vLine, ' '] ++ name ++ (if length name `mod` 2 == 0 then " " else "") ++ [' ', vLine]
    , "╰" ++ replicate (length name `div` 2 + 1) hLine ++
      (if isTop then "┬" else [hLine]) ++ replicate (length name `div` 2 + 1) hLine ++ "╯"
    ) <>
    go xs

mkArrow :: [(Int,Name)] -> Int -> Int -> Message -> [String]
mkArrow allBoxes from to (Message msg msgLen) = let (x,y) = go 0 allBoxes in [x,y]
  where
  go _ [] = mempty
  go c ((pre,name):xs) =
    ( topLine ++
      replicate (length name `div` 2 + 1) ' ' ++
      [' ', vLine, ' '] ++
      replicate (length name `div` 2 + 1) ' '
    , replicate (pre + length name `div` 2 + 1) before ++
      middle ++
      replicate (length name `div` 2 + 1) after
    ) <>
    go (succ c) xs
    where
      topLine
        | c == succ from && from < to = msg ++ replicate (pre - msgLen) ' '
        | c == from && from > to = replicate (pre - msgLen) ' ' ++ msg
        | otherwise = replicate pre ' '
      lower = min from to
      higher = max from to
      middle
        | c == from && from < to = [' ','├', hLine]
        | c == from && from > to = [hLine,'┤', ' ']
        | c == to && from < to = ['▶', vLine, ' ']
        | c == to && from > to = [' ', vLine, '◀']
        | otherwise = [active c, if isActive c then '┼' else vLine, active (succ c)]
      isActive x = lower < x && x <= higher
      active x | isActive x = hLine
               | otherwise = ' '
      before = active c
      after = active (succ c)

data Message = Message String Int

simpleMessage :: String -> Message
simpleMessage x = Message x (length x)

markedMessage :: String -> Message
markedMessage x = Message ("\x001b[93m[" <> x <> "]\x001b[0m") (2+length x)


data Arrow msg = Arrow
  { aFrom :: Name
  , aTo :: Name
  , aAt :: Int
  , aMessage :: msg
  }

generate :: [Arrow String] -> Int -> String
generate originalArrs current = unlines $
  mkBoxLines True names <>
  concat [ mkArrow names fromIndex toIndex msg
    | arr <- arrsMsg
    , let fromIndex = index (aFrom arr) names
          toIndex = index (aTo arr) names
          msg = aMessage arr
    ] <>
  mkBoxLines False names
  where
    arrsMsg = fmap (\arr -> if aAt arr == current
                     then arr {aMessage = markedMessage (aMessage arr)}
                     else arr {aMessage = simpleMessage (aMessage arr)})
                   originalArrs
    index _ [] = error "INTERNAL ERROR! can't find name"
    index k ((_,x):xs)
      | k == x = 0
      | otherwise = 1 + index k xs
    names = go (zip (repeat 0) (collectNames [] arrsMsg)) arrsMsg

    ensureSlack :: Int -> Int -> [(Int,Name)] -> [(Int,Name)]
    ensureSlack slack 0 ((p,x):xs) = (max slack p,x):xs
    ensureSlack slack n (x:xs) = x:ensureSlack slack (pred n) xs
    ensureSlack _slack _n [] = error "INTERNAL ERROR! can't ensure slack"

    add :: Name -> [Name] -> [Name]
    add x [] = [x]
    add x as@(k:xs)
      | k == x = as
      | otherwise = k:add x xs
    collectNames ns [] = ns
    collectNames ns (arr:arrs) = collectNames (add (aTo arr) $ add (aFrom arr) ns) arrs

    go :: [(Int, Name)] -> [Arrow Message] -> [(Int,Name)]
    go ns [] = ns
    go ns (arr:arrs) = go (ensureSlack msgLen pos ns) arrs
      where
        Message _ msgLen = aMessage arr
        fromIndex = index (aFrom arr) ns
        toIndex = index (aTo arr) ns

        pos | fromIndex < toIndex = succ fromIndex
            | otherwise = fromIndex

example :: Int -> String
example = generate arrs
  where
    arrs =
      [ Arrow "Client" "Dumblog" 0 "Append"
      , Arrow "Dumblog" "Backup" 1 "Write"
      , Arrow "Backup" "Dumblog" 2 "Ack"
      , Arrow "Dumblog" "Client" 3 "Ok"
      , Arrow "Backup" "Client" 4 "Test"
      ]
