module Timeline.Signals.Mappings where

import Timeline.Signals.Mappings.TimeSpaces (appendTimelineScopedExcept, removeTimelineScopedExcept)
import Timeline.UI.TimeSpace (TimeSpace(..)) as UI
import Timeline.UI.Timeline (Timeline(..)) as UI
import Timeline.UI.Event (Event) as UI
import Timeline.UI.TimeSpan (TimeSpan) as UI
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref
import Zeta.Types (READ, WRITE) as S
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (new, subscribeLight) as IxSignalMap
import Unsafe.Coerce (unsafeCoerce)


newtype Mappings = Mappings
  { timeSpaces :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpace
  , timelines :: IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) UI.Timeline
  , events :: IxSignalMap EventID ( read :: S.READ, write :: S.WRITE ) UI.Event
  , timeSpans :: IxSignalMap TimeSpanID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpan
  , root :: Ref (Maybe TimeSpaceID)
  }

getTimeSpacesMapping :: Mappings -> IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpace
getTimeSpacesMapping (Mappings {timeSpaces}) = timeSpaces

getTimelinesMapping :: Mappings -> IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) UI.Timeline
getTimelinesMapping (Mappings {timelines}) = timelines

getEventsMapping :: Mappings -> IxSignalMap EventID ( read :: S.READ, write :: S.WRITE ) UI.Event
getEventsMapping (Mappings {events}) = events

getTimeSpansMapping :: Mappings -> IxSignalMap TimeSpanID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpan
getTimeSpansMapping (Mappings {timeSpans}) = timeSpans

new :: Effect Mappings
new = do
  timeSpaces <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  timelines <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  events <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  timeSpans <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  -- can update timelines or siblings
  let
    handleTimeSpacesMappingUpdate :: Tuple TimeSpaceID (MapUpdate UI.TimeSpace) -> Effect Unit
    handleTimeSpacesMappingUpdate (Tuple _ mapUpdate) = case mapUpdate of
      -- Via Insert, you know that all instances will be "new", to the universe of sets
      -- FIXME siblings and timelines should already exist, and if not, they will by the time lookups happen
      MapInsert _ -> pure unit
      -- FIXME find and update timelines / siblings parent
      MapUpdate { valueNew: UI.TimeSpace { timelines: timelines', siblings } } -> pure unit
      -- FIXME delete children
      MapDelete { valueOld: UI.TimeSpace { timelines: timelines', siblings } } -> pure unit
  IxSignalMap.subscribeLight "UISets" handleTimeSpacesMappingUpdate timeSpaces
  let
    handleTimelinesMappingUpdate :: Tuple TimelineID (MapUpdate UI.Timeline) -> Effect Unit
    handleTimelinesMappingUpdate (Tuple id mapUpdate) = case mapUpdate of
      -- FIXME children should already exist, and if not, they will by the time lookups happen
      MapInsert { valueNew: UI.Timeline { timeSpace } } -> void (appendTimelineScopedExcept [ "UISets" ] id timeSpace timeSpaces)
      MapUpdate { valueOld: UI.Timeline { children: childrenOld, timeSpace: timeSpaceOld }, valueNew: UI.Timeline { children: childrenNew, timeSpace: timeSpaceNew } } ->
        -- Implements movement of self, FIXME ignores children's parent references
        if timeSpaceOld == timeSpaceNew then
          pure unit
        else do
          void (removeTimelineScopedExcept [ "UISets" ] id timeSpaceOld timeSpaces)
          void (appendTimelineScopedExcept [ "UISets" ] id timeSpaceNew timeSpaces)
      -- removes self from parent
      MapDelete { valueOld: UI.Timeline { timeSpace } } -> void (removeTimelineScopedExcept [ "UISets" ] id timeSpace timeSpaces)
  -- FIXME finish rest of mapping updates
  root <- Ref.new Nothing
  pure
    (Mappings { timeSpaces, timelines, events, timeSpans, root })

