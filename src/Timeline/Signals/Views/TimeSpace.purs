module Timeline.Signals.Views.TimeSpace where

import Timeline.UI.TimeSpace (TimeSpace)
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.Signals.Mappings.TimeSpaces (getTimeSpaceScoped, addTimeSpaceForceScopedExcept)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array (last) as Array
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
import IxZeta (IxSignal)
import IxZeta (make, setExcept, get, subscribeLight) as IxSignal
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (subscribeLight) as IxSignalMap

-- | Utility function that fetches the "most relevant" time space id, rather than using
-- | the trie series of indicies.
getCurrentTimeSpaceID ::
  { timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect TimeSpaceID
getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef } = do
  timeSpaceIDs <- IxSignal.get timeSpaceSelectedSignal
  case Array.last timeSpaceIDs of
    Just last -> pure last
    Nothing -> do
      mRoot <- Ref.read rootRef
      case mRoot of
        Nothing -> throw "No root TimeSpaceID"
        Just root -> pure root

getCurrentTimeSpace ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect TimeSpace
getCurrentTimeSpace { timeSpaceSelectedSignal, rootRef, timeSpacesMapping } = do
  currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
  eTimeSpace <- getTimeSpaceScoped currentTimeSpaceID timeSpacesMapping
  case eTimeSpace of
    Left e -> throw $ "Cannot getTimeSpaceScoped - error: " <> show e -- FIXME format errors
    Right x -> pure x

-- TODO decouple and format like Timeline.Signals.Views.Timelines

-- | Create the "viewed time space" signal on boot, where the initial
-- | time space is synthesized by `UISets`.
newTimeSpaceSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpace)
newTimeSpaceSignal { timeSpacesMapping, timeSpaceSelectedSignal, rootRef } = do
  currentTimeSpace <-
    getCurrentTimeSpace
      { timeSpacesMapping: S.readOnly timeSpacesMapping
      , timeSpaceSelectedSignal
      , rootRef
      }
  sig <- IxSignal.make currentTimeSpace
  let
    -- Fires whenever we need to reset the most up-to-date timeSpace
    getNewTimeSpace :: Effect Unit
    getNewTimeSpace = do
      newTimeSpace <-
        getCurrentTimeSpace
          { timeSpacesMapping: S.readOnly timeSpacesMapping
          , timeSpaceSelectedSignal
          , rootRef
          }
      IxSignal.setExcept [ "TimeSpaceSignal" ] newTimeSpace sig

  -- Get the latest signal whenever the selected signal changes (via explore timeSpaces)
  IxSignal.subscribeLight "TimeSpaceSignal" (const getNewTimeSpace) timeSpaceSelectedSignal
  let
    -- Fires whenever an update happens to the timeSpaces mapping database
    handleMappingUpdate :: Tuple TimeSpaceID (MapUpdate TimeSpace) -> Effect Unit
    handleMappingUpdate (Tuple updatedTimeSpaceID updatedTimeSpace) = do
      currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
      if currentTimeSpaceID /= updatedTimeSpaceID then
        -- Not the one being viewed, don't do anything
        pure unit
      else case updatedTimeSpace of
        -- Strange case where we're viewing the timeSpace before it was added to the database...
        -- it's safe to just re-set our view of the timeSpace, just to be sure we have the most
        -- up-to-date version.
        MapInsert { valueNew } -> IxSignal.setExcept [ "TimeSpaceSignal" ] valueNew sig

        -- Updated the one we're viewing - this is the most important case to consider.
        MapUpdate { valueNew } -> IxSignal.setExcept [ "TimeSpaceSignal" ] valueNew sig

        -- Strange case where the timeSpace we're currently viewing just got deleted.
        -- FIXME Should we just wait and continue viewing the fragmented data?
        -- Or should we navigate to a higher timeSpace?
        -- FIXME what if root gets deleted while viewing it?
        MapDelete _ -> pure unit
  IxSignalMap.subscribeLight "TimeSpaceSignal" handleMappingUpdate timeSpacesMapping

  let
    -- Overwrite the already existing timeSpace in the database when the viewed one updates -
    -- i.e., when the user modifies it in the user interface.
    handleSelfUpdate :: TimeSpace -> Effect Unit
    handleSelfUpdate timeSpace = addTimeSpaceForceScopedExcept [ "TimeSpaceSignal" ] timeSpace (S.writeOnly timeSpacesMapping)
  -- FIXME dereference Timelines and Siblings when necessary
  IxSignal.subscribeLight "TimeSpaceSignal" handleSelfUpdate sig

  pure sig
