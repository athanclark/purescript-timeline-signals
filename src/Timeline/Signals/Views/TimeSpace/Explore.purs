module Timeline.Signals.Views.TimeSpace.Explore where

import Timeline.Signals.Mappings.Errors (SynthesizeError (NoRootExists))
import Timeline.Signals.Mappings.TimeSpaces (getTimeSpaceScoped)
import Timeline.Signals.Mappings.TimeSpans (getTimeSpanScoped)
import Timeline.Signals.Mappings.Timelines (getTimelineScoped)
import Timeline.UI.TimeSpace (TimeSpace (..))
import Timeline.UI.TimeSpace.Explore (ExploreTimeSpaces (..))
import Timeline.UI.TimeSpace.TimeScale (TimeScale (..))
import Timeline.UI.TimeSpan (TimeSpan (..))
import Timeline.UI.Timeline (Timeline (..))
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly (..))
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.ID.Event (EventID)
import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either(..))
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (unsafeMapMaybe, unsafeTraverse, unsafeSequence, unsafeConcatMap, unsafeAppend) as UniqueArray
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import Effect.Exception (throw)
import Zeta.Types (READ, readOnly) as S
import IxZeta (IxSignal)
import IxZeta (make, set) as IxSignal
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (subscribeLight) as IxSignalMap

-- | Returns a read-only signal, because nobody should be writing to this signal -
-- | it only changes when constituents change.
newExploreTimeSpacesSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpansMapping :: IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignal ( read :: S.READ ) ExploreTimeSpaces)
newExploreTimeSpacesSignal { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef } = do
  initialExplore <- getLatestExploreTimeSpaces
  sig <- IxSignal.make initialExplore
  let
    updateExploreTimeSpaces = do
      newExplore <- getLatestExploreTimeSpaces
      IxSignal.set newExplore sig
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timeSpacesMapping
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timelinesMapping
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timeSpansMapping
  pure (S.readOnly sig)
  where
  getLatestExploreTimeSpaces :: Effect ExploreTimeSpaces
  getLatestExploreTimeSpaces = do
    eExplore <- synthesizeExploreTimeSpacesScoped { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef }
    case eExplore of
      Left e -> throw $ "Couldn't synthesize explore time space: " <> show e
      Right x -> pure x

synthesizeExploreTimeSpacesScoped ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpansMapping :: IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (Either SynthesizeError ExploreTimeSpaces)
synthesizeExploreTimeSpacesScoped { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef } = do
  mRoot <- Ref.read rootRef
  case mRoot of
    Nothing -> pure (Left NoRootExists)
    Just id -> go id
  where
  go :: TimeSpaceID -> Effect (Either SynthesizeError ExploreTimeSpaces)
  go timeSpaceId = do
    eTimeSpace <- getTimeSpaceScoped timeSpaceId timeSpacesMapping
    case eTimeSpace of
      Left e -> pure (Left e)
      Right
        ( TimeSpace
          { title
        , timeScale: TimeScale { limit }
        , id
        , timelines
        , siblings
        }
      ) -> do
        let
          getSpans :: UniqueArray (EventOrTimeSpanPoly EventID TimeSpanID) -> UniqueArray TimeSpanID
          getSpans = UniqueArray.unsafeMapMaybe getSpan
            where
            getSpan (EventOrTimeSpanPoly eOrTs) = case eOrTs of
              Left _ -> Nothing
              Right ts -> Just ts
        eSiblings <- UniqueArray.unsafeTraverse (flip getTimeSpanScoped timeSpansMapping) (getSpans siblings)
        case UniqueArray.unsafeSequence eSiblings of
          Left e -> pure (Left e)
          Right siblings' -> do
            eTimelines <- UniqueArray.unsafeTraverse (flip getTimelineScoped timelinesMapping) timelines
            case UniqueArray.unsafeSequence eTimelines of
              Left e -> pure (Left e)
              Right timelines' -> do
                let
                  childrenOfTimelines :: UniqueArray TimeSpanID
                  childrenOfTimelines =
                    UniqueArray.unsafeConcatMap
                      (\(Timeline { children }) -> getSpans children)
                      timelines'
                eChildren <- UniqueArray.unsafeTraverse (flip getTimeSpanScoped timeSpansMapping) childrenOfTimelines
                case UniqueArray.unsafeSequence eChildren of
                  Left e -> pure (Left e)
                  Right children' -> do
                    let
                      siblingsWithTimeSpaces :: UniqueArray TimeSpaceID
                      siblingsWithTimeSpaces = UniqueArray.unsafeMapMaybe (\(TimeSpan { timeSpace }) -> timeSpace) siblings'

                      childrenWithTimeSpaces :: UniqueArray TimeSpaceID
                      childrenWithTimeSpaces = UniqueArray.unsafeMapMaybe (\(TimeSpan { timeSpace }) -> timeSpace) children'
                    -- recurse
                    eDescendants <- UniqueArray.unsafeTraverse go (UniqueArray.unsafeAppend siblingsWithTimeSpaces childrenWithTimeSpaces)
                    case UniqueArray.unsafeSequence eDescendants of
                      Left e -> pure (Left e)
                      Right children ->
                        pure
                          $ Right
                          $ ExploreTimeSpaces
                              { title
                              , limit
                              , id
                              , children
                              }
