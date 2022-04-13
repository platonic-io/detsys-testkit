{-# LANGUAGE ExistentialQuantification #-}

module ATMC.Lec5.Configuration where

import Data.Typeable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import ATMC.Lec5.Codec
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

newtype Configuration = Configuration (IntMap SomeCodecSM)

lookupReceiver :: NodeId -> Configuration -> Maybe SomeCodecSM
lookupReceiver (NodeId nid) (Configuration im) = IntMap.lookup nid im

updateReceiverState :: Typeable state
                    => NodeId -> Configuration -> state -> Configuration
updateReceiverState (NodeId nid) (Configuration im) newState =
  Configuration (IntMap.update (updateState newState) nid im)

makeConfiguration :: [SomeCodecSM] -> Configuration
makeConfiguration = Configuration . IntMap.fromList . zip [0..]

data SomeCodecSM = forall state request message response. Typeable state =>
                   SomeCodecSM (Codec request message response)
                               (SM state request message response)

updateState :: Typeable s => s -> SomeCodecSM -> Maybe SomeCodecSM
updateState newState' (SomeCodecSM codec (SM _oldState step timeout)) =
  case cast newState' of
    Just newState -> Just (SomeCodecSM codec (SM newState step timeout))
    Nothing       -> Nothing
