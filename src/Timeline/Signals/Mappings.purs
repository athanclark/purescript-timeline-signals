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


-- | The "source of truth" for all entities in a given timeline - whether they're bound
-- | to other entities or not, this should be seen as a "database" for all entities.
-- |
-- | Converting this to a document can be done with `MappingsM`, and likewise derivative
-- | _view_ signals depend on this for interaction with segments of a timeline in the user interface.
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

getRootRef :: Mappings -> Ref (Maybe TimeSpaceID)
getRootRef (Mappings {root}) = root

-- | Create a new, empty `Mappings` database, with each entity collection _interlinked_
-- | to relay appropriate changes to related entities, when necessary.
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
      -- NOTE siblings and timelines should already exist, and if not, they will by the time lookups happen
      MapInsert { valueNew: UI.TimeSpace { parent } } -> case parent of
        Nothing -> pure unit
        Just parent' -> pure unit -- FIXME check if timeSpace already has a child - or, if there's a cycle!
      -- FIXME find and update timelines / siblings parent references
      -- valueOld timelines and siblings that aren't included in value new shouldn't have a dangling parent reference - assign it to Nothing
      -- valueNew timelines and siblings should be assigned the valueNew id as parent (if it changed, if not, just the ones newly included)
      MapUpdate { valueNew: UI.TimeSpace { timelines: timelines', siblings } } -> pure unit
      -- FIXME old timelines and siblings shouldn't have dangling parent references - set them to Nothing
      MapDelete { valueOld: UI.TimeSpace { timelines: timelines', siblings } } -> pure unit
  IxSignalMap.subscribeLight "UISets" handleTimeSpacesMappingUpdate timeSpaces

  let
    handleTimelinesMappingUpdate :: Tuple TimelineID (MapUpdate UI.Timeline) -> Effect Unit
    handleTimelinesMappingUpdate (Tuple id mapUpdate) = case mapUpdate of
      -- NOTE children should already exist, and if not, they will by the time lookups happen
      MapInsert { valueNew: UI.Timeline { parent } } -> case parent of
        -- Do nothing when there's no parent reference in the timeline
        Nothing -> pure unit
        Just parent' -> void (appendTimelineScopedExcept [ "UISets" ] id parent' timeSpaces)
      MapUpdate
        { valueOld: UI.Timeline { children: childrenOld, parent: timeSpaceOld }
        , valueNew: UI.Timeline { children: childrenNew, parent: timeSpaceNew }
        } ->
        -- Implements movement of self
        case Tuple timeSpaceOld timeSpaceNew of
          Tuple Nothing Nothing -> pure unit
          Tuple (Just timeSpaceOld') (Just timeSpaceNew')
            | timeSpaceOld' == timeSpaceNew' -> pure unit
            | otherwise -> do
            void (removeTimelineScopedExcept [ "UISets" ] id timeSpaceOld' timeSpaces)
            void (appendTimelineScopedExcept [ "UISets" ] id timeSpaceNew' timeSpaces)
          Tuple Nothing (Just timeSpaceNew') ->
            void (appendTimelineScopedExcept [ "UISets" ] id timeSpaceNew' timeSpaces)
          Tuple (Just timeSpaceOld') Nothing ->
            void (removeTimelineScopedExcept [ "UISets" ] id timeSpaceOld' timeSpaces)
      -- removes self from parent
      MapDelete { valueOld: UI.Timeline { parent } } -> case parent of
        Nothing -> pure unit
        Just parent' -> void (removeTimelineScopedExcept [ "UISets" ] id parent' timeSpaces)
  -- TODO finish rest of mapping updates

  root <- Ref.new Nothing
  pure
    (Mappings { timeSpaces, timelines, events, timeSpans, root })

