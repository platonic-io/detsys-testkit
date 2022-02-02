## Testing Linearisability

```haskell
type Command
type Response
type Pid

type Model
step :: Model -> Command -> (Model, Response)
step = ..

initModel :: Model
initModel = ..

data ConcurrentEvent
  = Invoke Pid Command
  | Return Pid Command (Maybe Response) -- Nothing means :INFO

data LinearEvent
  = LinearEvent Pid Command Response

isLinearOf :: [LinearEvent] -> [ConcurrentEvent] -> Bool
isLinearOf = ..

isValid :: [LinearEvent] -> Model -> Bool
isValid = ..

-- function we want to test
linearize :: [ConcurrentEvent] -> Bool {- Maybe return the [LinearEvent]? -}
linearize = ..

genCommand :: Gen Command
genCommand = ..

data PidStatus
  = DoingNothing
  | MadeRequest Command
  | CommitedRequest Command (Maybe Response)
  | FailedRequest Command

genHistory :: [Pid] -> Gen ([ConcurrentEvent], [LinearEvent])
genHistory pids = sized $ \s -> go [] [] initModel s $ zip pids DoingNothing
  where
    go conc linear model size [] = pure (reverse conc, reverse linear)
    go conc linear model size pids = do
      (pid, state, pids') <- selectOne pids
      case state of
        DoingNothing | size <= 0 -> 
          -- we have done enough, no need to generate more
          go conc linear pids'
        DoingNothing -> do
          command <- genCommand
          go (Ivoke pid command:conc) linear (pred size) ((pid, MadeRequest command):pids')
        MadeRequest cmd -> oneOf 
          [ do -- request succeed, and response succeeded
            let (model', resp) = step model cmd
            go conc (LinearEvent pid cmd resp:linear) model' size ((pid, CommitedRequest cmd (Just resp)):pids')
          , do -- request succeed, but response failed
            let (model', resp) = step model cmd
            go conc (LineareEvent pid cmd resp:linear) model' size ((pid, ComittedRequest cmd Nothing):pids')
          , do -- request fails
            go conc linear model size ((pid, FailedRequest cmd):pids')
          ]
        ComittedRequest cmd mresp ->
          go (Return pid cmd mresp:conc) linear model size $ (pid,DoingNothing):pids'
        FailedRequest cmd ->
          go (Return pid cmd Noting:conc) linear model size $ (pid, DoingNothing):pids'


prop_SanityCheck :: [Pid] -> Property
prop_SanityCheck pids =
  forAll (genHistory pids) $ \ (ch, lh) ->
    lh `isLinearOf` ch

prop_SanityCheck2 :: [Pid] -> Property
prop_SanityCheck2 pids =
  forAll (genHistory pids) $ \ (_, lh) ->
    isValid lh initModel
    
prop_Linearize :: [Pid] -> Property
prop_Linearize pids =
  forAll (genHistory pids) $ \ (ch, _) ->
    linearize ch -- or if it produces evidence check that they are correct
```

Currently not checking that `linearize` would not accept history it shouldn't

Also difficult to `shrink` using this approach
