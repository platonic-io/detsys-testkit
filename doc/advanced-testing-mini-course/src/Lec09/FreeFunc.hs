{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Categories #-}

{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Lec09.FreeFunc where

-- http://conal.net/papers/compiling-to-categories/

-- [Deconstructing Lambdas—An Awkward Guide to Programming Without
-- Functions](https://www.youtube.com/watch?v=xZmPuz9m2t0)

-- https://github.com/ChrisPenner/catalyst

import qualified Control.Arrow            as Arrow
import           Control.Category         (Category, id, (.))
import           Data.Type.Equality
import           Overloaded.Categories
import           Prelude                  hiding (id, (.))
import qualified Prelude
import           Test.QuickCheck
import           Text.Read                (readMaybe)

import           Control.Exception
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Posix.Files

------------------------------------------------------------------------

data FreeFunc a b where
  Id      :: FreeFunc a a
  Compose :: FreeFunc b c -> FreeFunc a b -> FreeFunc a c
  Copy    :: FreeFunc a (a, a)
  Consume :: FreeFunc a ()
  First   :: FreeFunc a b -> FreeFunc (a, x) (b, x)
  Second  :: FreeFunc a b -> FreeFunc (x, a) (x, b)
  Fst     :: FreeFunc (a, b) a
  Snd     :: FreeFunc (a, b) b
  Distr   :: FreeFunc (Either a b, c) (Either (a, c) (b, c))
  Inl     :: FreeFunc a (Either a b)
  Inr     :: FreeFunc b (Either a b)
  Case    :: FreeFunc a c -> FreeFunc b c -> FreeFunc (Either a b) c
  App     :: FreeFunc (a -> b, a) b
  Curry   :: FreeFunc (a, b) c -> FreeFunc a (b -> c)
  Const   :: (IsValue a, Show a) => a -> FreeFunc x a
  Add     :: Num a => FreeFunc (a, a) a

deriving instance Show (FreeFunc a b)

instance Category FreeFunc where
  id  = Id
  (.) = Compose

returnA :: FreeFunc a a
returnA = Id

infixr 1 >>>
(>>>) :: FreeFunc a b -> FreeFunc b c -> FreeFunc a c
(>>>) = flip Compose

infixr 3 ***
(***) :: FreeFunc a c -> FreeFunc b d -> FreeFunc (a, b) (c, d)
f *** g = First f >>> Second g

infixr 3 &&&
(&&&) :: FreeFunc a b -> FreeFunc a c -> FreeFunc a (b, c)
f &&& g = Copy >>> f *** g

infixr 2 |||
(|||) :: FreeFunc b d -> FreeFunc c d -> FreeFunc (Either b c) d
(|||) = Case

------------------------------------------------------------------------

instance CategoryWith1 (FreeFunc) where
  type Terminal (FreeFunc) = ()

  terminal :: FreeFunc a ()
  terminal = Consume

instance CartesianCategory (FreeFunc) where
  type Product (FreeFunc) = (,)

  proj1 = Fst
  proj2 = Snd

  fanout :: FreeFunc a b -> FreeFunc a c -> FreeFunc a (b, c)
  fanout = (&&&)

instance CocartesianCategory (FreeFunc) where
  type Coproduct (FreeFunc) = Either

  inl = Inl
  inr = Inr

  fanin :: FreeFunc a c -> FreeFunc b c -> FreeFunc (Either a b) c
  fanin = (|||)

instance BicartesianCategory (FreeFunc) where
  distr :: FreeFunc (Either a b, c) (Either (a, c) (b, c))
  distr = Distr

instance CCC (FreeFunc) where
  type Exponential (FreeFunc) = (->)

  eval :: FreeFunc (a -> b, a) b
  eval = App

  transpose :: FreeFunc (a, b) c -> FreeFunc a (b -> c)
  transpose = Curry

instance GeneralizedElement (FreeFunc) where
  type Object (FreeFunc) a = a

  konst :: a -> FreeFunc x a
  konst = undefined
  -- XXX: doens't type check because of `IsValue` constraint in `Const`, not
  -- sure how important this instance is for the arrow desugaring though? It
  -- seems just using `Const` directly inside the arrow notation is fine.

------------------------------------------------------------------------

-- [The Categorical Abstract Machine](https://core.ac.uk/download/pdf/82453178.pdf)
-- https://bartoszsypytkowski.com/simple-virtual-machine/

type Code = [Instruction]

data Instruction
  = ID | FST | SND | COPY | CONSUME | FIRST | SECOND | CONS
  | SWAP | QUOTE Value | ADD | APP | CURRY Code | INL | INR
  | BRANCH Code Code | RETURN | PUSH | ROT
  deriving (Eq, Show, Read)

data Value = Unit | Pair Value Value | L Value | R Value | Int Int | Closure Code Value
  deriving (Eq, Show, Read)

compile :: FreeFunc a b -> Code
compile Id            = [ID]
compile (Compose g f) = compile f ++ compile g
compile Copy          = [COPY]
compile Consume       = [CONSUME]
compile (First f)     = [FIRST]  ++ compile f ++ [SWAP, CONS]
compile (Second f)    = [SECOND] ++ compile f ++ [CONS]
compile Inl           = [INL]
compile Inr           = [INR]
compile (Case l r)    = [BRANCH (compile l ++ [RETURN]) (compile r ++ [RETURN])]
compile Add           = [ADD]
compile Fst           = [FST]
compile Snd           = [SND]
compile App           = [APP]
compile (Curry p)     = [CURRY (compile p)]
compile (Const x)     = [QUOTE (quote x)]
compile Distr         = [PUSH, FST, BRANCH [ROT, SWAP, SND, CONS, INL, RETURN] [ROT, SWAP, SND, CONS, INR, RETURN]]

type Stack = [StackElement]

data StackElement = Value Value | Code Code
  deriving Show

type Config = (Value, Code, Stack)

exec1 :: Config -> Either Config Config
exec1 (x,                     ID          :  is, s)           = Right (x, is, s)
exec1 (Pair l _r,             FST         :  is, s)           = Right (l, is, s)
exec1 (Pair _l r,             SND         :  is, s)           = Right (r, is, s)
exec1 (x,                     COPY        :  is, s)           = Right (Pair x x, is, s)
exec1 (_x,                    CONSUME     :  is, s)           = Right (Unit, is, s)
exec1 (Pair a x,              FIRST       :  is, s)           = Right (a, is, Value x : s)
exec1 (Pair x b,              SECOND      :  is, s)           = Right (b, is, Value x : s)
exec1 (a,                     CONS        :  is, Value x : s) = Right (Pair x a, is, s)
exec1 (a,                     SWAP        :  is, Value b : s) = Right (b, is, Value a : s)
exec1 (x,                     PUSH        :  is, s)           = Right (x, is, Value x : s)
exec1 (x,                     ROT         :  is, y : z : s)   = Right (x, is, z : y : s)
exec1 (x,                     RETURN      : _is, Code c : s)  = Right (x, c, s)
exec1 (l,                     INL         :  is, s)           = Right (L l, is, s)
exec1 (r,                     INR         :  is, s)           = Right (R r, is, s)
exec1 (L l,                   BRANCH c _d :  is, s)           = Right (l, c, Code is : s)
exec1 (R r,                   BRANCH _c d :  is, s)           = Right (r, d, Code is : s)
exec1 (_a,                    QUOTE v     :  is, s)           = Right (v, is, s)
exec1 (Pair (Int i) (Int j),  ADD         :  is, s)           = Right (Int (i + j), is, s)
exec1 (Pair (Closure x y) z,  APP         :  is, s)           = Right (Pair y z, x, Code is : s)
exec1 (x,                     CURRY c     :  is, s)           = Right (Closure c x, is, s)
exec1 config                                                  = Left config

exec :: Config -> Config
exec cfg = case exec1 cfg of
  Right cfg' -> exec cfg'
  Left  cfg' -> cfg'

debugExec :: Config -> IO Config
debugExec cfg = case exec1 cfg of
  Right cfg' -> do
    print cfg'
    debugExec cfg'
  Left  cfg' -> return cfg'

run :: FreeFunc a b -> (Value -> Value)
run f x = y
  where
    (y, _, _) = exec (x, compile f, [])

debug :: FreeFunc a b -> (Value -> Config)
debug f x = exec (x, compile f, [])

catAssoc :: CartesianCategory cat
         => cat (Product cat (Product cat a b) c) (Product cat a (Product cat b c))
catAssoc = proc ((x, y), z) -> identity -< (x, (y, z))

catAssoc' :: FreeFunc ((a, b), c) (a, (b, c))
catAssoc' = proc ((x, y), z) -> identity -< (x, (y, z))

t :: Bool
t = run catAssoc ((Int 1 `Pair` Int 2) `Pair` Int 3) == (Int 1 `Pair` (Int 2 `Pair` Int 3))

interpret :: FreeFunc a b -> (a -> b)
interpret func = case func of
  Id          -> id
  Compose g f -> interpret g . interpret f
  Fst         -> fst
  Snd         -> snd
  Distr       -> \(e, z) -> case e of
                   Left  x -> Left (x, z)
                   Right y -> Right (y, z)
  First  f    -> Arrow.first  (interpret f)
  Second f    -> Arrow.second (interpret f)
  Copy        -> \x -> (x, x)
  Consume     -> const ()
  Curry f     -> curry (interpret f)
  App         -> uncurry ($)
  Case f g    -> interpret f Arrow.||| interpret g
  Inl         -> Left
  Inr         -> Right
  Add         -> uncurry (+)
  Const x     -> const x

interpretMany :: FreeFunc (i, s) (o, s) -> [i] -> s -> ([o], s)
interpretMany f is s0 = go is [] s0
  where
    g = interpret f

    go []       os s = (reverse os, s)
    go (i : is) os s = let (o, s') = g (i, s) in go is (o : os) s'

class IsValue a where
  quote :: a -> Value

instance IsValue () where
  quote () = Unit

instance (IsValue a, IsValue b) => IsValue (a, b) where
  quote (x, y) = Pair (quote x) (quote y)

instance IsValue Int where
  quote = Int

instance (IsValue a, IsValue b) => IsValue (Either a b) where
  quote (Left  x) = L (quote x)
  quote (Right y) = R (quote y)

prop_commute :: (IsValue a, IsValue b) => FreeFunc a b -> a -> Bool
prop_commute f x = quote (interpret f x) == run f (quote x)
-- ^ TODO: Generate `FreeFunc a b`...


-- counter :: (CartesianCategory cat) => cat (Product cat (Either () ()) Int) (Product cat () Int)
counter :: FreeFunc (Either () (), Int) (Either () Int, Int)
counter = proc (i, n) ->
  case i of
    Left l -> do
      one <- Const 1 -< ()
      n' <- Add -< (n, one)
      returnA -< (Left (), n')
    Right r -> do
      returnA -< (Right n, n)

counterV2 :: FreeFunc (Either () (), Int) (Either () Int, Int)
counterV2 = proc (i, n) ->
  case i of
    Left l -> do
      two <- Const 2 -< ()
      n'  <- Add -< (n, two)
      returnA -< (Left (), n')
    Right r -> do
      returnA -< (Right n, n)


t2 = debug counter (Pair (L Unit) (Int 0))
t3 = debug Distr (L Unit `Pair` Int 1)
t4 = run Distr (R Unit `Pair` Int 1)
t5 = interpret Distr (Left (), 1)

------------------------------------------------------------------------

data Command = Do Value | Load Code
  deriving (Show, Read)

pipePath :: FilePath
pipePath = "/tmp/freefunc"

main :: Code -> IO ()
main code = do
  safeCreateNamedPipe (pipePath <.> "command")
  safeCreateNamedPipe (pipePath <.> "response")
  withFile (pipePath <.> "command") ReadWriteMode $ \h -> do
    hSetBuffering h LineBuffering
    go h code (Int 0)
  where
    go :: Handle -> Code -> Value -> IO ()
    go h code state = do
      s <- hGetLine h
      let mCmd = readMaybe s
      case mCmd of
        Nothing -> do
          putStrLn ("Invalid command: " ++ s)
          go h code state
        Just (Load code') -> do
          respond "Upgraded!"
          go h code' state
        Just (Do input) -> do
          let (Pair output state', _, _) = exec (Pair input state, code, [])
          respond (show output)
          go h code state'

respond :: String -> IO ()
respond s =
  withFile (pipePath <.> "response") WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    hPutStrLn h s

tick :: IO ()
tick = writeFile (pipePath <.> "command") (show (Do (L Unit)) ++ "\n")

get :: IO ()
get = writeFile (pipePath <.> "command") (show (Do (R Unit)) ++ "\n")

load :: Code -> IO ()
load code = writeFile (pipePath <.> "command") (show (Load code) ++ "\n")

safeCreateNamedPipe :: FilePath -> IO ()
safeCreateNamedPipe fp =
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return

------------------------------------------------------------------------

-- https://wiki.haskell.org/QuickCheck_/_GADT

data Dynamic = forall t. (IsValue t, Show t) => Dynamic (Ty t)

data Ty :: * -> * where
  TValue :: (IsValue a, Show a) => a -> Ty a
  -- TFun   :: (IsValue a, IsValue b, Show a, Show b) => Ty a -> Ty b -> Ty (a -> b)
  TPair  :: Ty a -> Ty b -> Ty (a, b)

tyEq :: Ty a -> Ty b -> Maybe (a :~: b)
tyEq = undefined

genTy :: Gen Dynamic
genTy = undefined

data SomeFreeFunc = forall a b. (IsValue a, IsValue b, Show a, Show b) => SomeFreeFunc (FreeFunc a b)

genValue :: IsValue a => Ty a -> Gen a
genValue = undefined

genFreeFunc :: Gen SomeFreeFunc
genFreeFunc = do
  Dynamic a <- genTy
  Dynamic b <- genTy
  SomeFreeFunc <$> genFun a b

genFun :: (IsValue a, IsValue b, Show a, Show b) => Ty a -> Ty b -> Gen (FreeFunc a b)
-- genFun g@(TFun b c) f@(TFun a b') = case tyEq b b' of
--   Just Refl -> do
--     g' <- genFun b c
--     f' <- genFun a b'
--     return _ -- (Compose g' f')
genFun (TPair l r) b = case (tyEq l b, tyEq r b) of
  (Just Refl, Just Refl) -> frequency [(5, return Fst), (5, return Snd), (1, Const <$> genValue b)] -- XXX: only generate const if b is TValue?
  (Just Refl, Nothing)   -> frequency [(9, return Fst), (1, Const <$> genValue b)]
  (Nothing, Just Refl)   -> frequency [(9, return Snd), (1, Const <$> genValue b)]
  (Nothing, Nothing)     -> Const <$> genValue b

{- ghc: panic! (the 'impossible' happened)
  (GHC version 9.0.2:
        No skolem info:
  [a1_aapJ[sk:1]]
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/GHC/Utils/Outputable.hs:1230:37 in ghc:GHC.Utils.
Outputable
        pprPanic, called at compiler/GHC/Tc/Errors.hs:2859:17 in ghc:GHC.Tc.Errors
  case a of
    TFun (TPair l r) -> case (tyEq l b, tyEq r b) of
      -- (Just Refl, Just Refl) -> frequency [(3, return Fst), (3, return Snd), (1, Const <$> genValue b)]
      (Just Refl, Nothing) -> undefined
      (Nothing, Just Refl) -> undefined
      (Nothing, Nothing) -> undefined
    -- TValue x -> return (SomeFreeFunc (Const x))
-}

------------------------------------------------------------------------

-- https://github.com/turion/essence-of-live-coding
