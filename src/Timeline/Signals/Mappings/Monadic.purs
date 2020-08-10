module Timeline.Signals.Mappings.Monadic where

import Timeline.Signals.Mappings (Mappings (..), getTimeSpacesMapping, getTimelinesMapping, getEventsMapping, getTimeSpansMapping)
import Timeline.Signals.Mappings.TimeSpaces (getTimeSpaceScoped, addTimeSpaceScoped, addTimeSpaceForceScoped)
import Timeline.Signals.Mappings.Timelines (getTimelineScoped, addTimelineScoped)
import Timeline.Signals.Mappings.Events (getEventScoped, addEventScoped)
import Timeline.Signals.Mappings.TimeSpans (getTimeSpanScoped, addTimeSpanScoped)
import Timeline.Signals.Mappings.EventsOrTimeSpans (getEventOrTimeSpanScoped)
import Timeline.Signals.Mappings.Errors (PopulateError, SynthesizeError)
import Timeline.UI.TimeSpace (TimeSpace(..)) as UI
import Timeline.UI.Timeline (Timeline(..)) as UI
import Timeline.UI.Event (Event(..)) as UI
import Timeline.UI.TimeSpan (TimeSpan(..)) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly, EventOrTimeSpan) as UI
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Effect (Effect)
import Effect.Ref (write, read) as Ref
import Effect.Class (class MonadEffect, liftEffect)
import Zeta.Types (readOnly, writeOnly) as S
import IxZeta.Map (assign) as IxSignalMap

-- | Builder / Reader monad - to be used to either build a mapping from a document, or a document from a mapping.
newtype MappingsM e a
  = MappingsM (ReaderT Mappings (ExceptT e Effect) a)

derive newtype instance functorMappingsM :: Functor (MappingsM e)

derive newtype instance applyMappingsM :: Apply (MappingsM e)

derive newtype instance applicativeMappingsM :: Applicative (MappingsM e)

derive newtype instance bindMappingsM :: Bind (MappingsM e)

derive newtype instance monadMappingsM :: Monad (MappingsM e)

derive newtype instance monadEffectMappingsM :: MonadEffect (MappingsM e)

derive newtype instance monadThrowMappingsM :: MonadThrow e (MappingsM e)

derive newtype instance monadAskMappingsM :: MonadAsk Mappings (MappingsM e)

runMappingsM :: forall e a. MappingsM e a -> Mappings -> Effect (Either e a)
runMappingsM (MappingsM x) s = runExceptT (runReaderT x s)

asMappingsM :: forall e a p q. (Mappings -> q) -> (p -> q -> Effect (Either e a)) -> p -> MappingsM e a
asMappingsM fromMappings f p = do
  uiSets <- ask
  let
    q = fromMappings uiSets
  eX <- liftEffect (f p q)
  case eX of
    Left e -> throwError e
    Right x -> pure x

asMappingsM' :: forall e a q. (Mappings -> q) -> (q -> Effect (Either e a)) -> MappingsM e a
asMappingsM' fromMappings f = do
  uiSets <- ask
  let
    q = fromMappings uiSets
  eX <- liftEffect (f q)
  case eX of
    Left e -> throwError e
    Right x -> pure x

-- | Includes an already flat time space - doesn't verify constituents
addTimeSpace :: UI.TimeSpace -> MappingsM PopulateError Unit
addTimeSpace = asMappingsM (S.writeOnly <<< getTimeSpacesMapping) (\p q -> maybeToEither <$> addTimeSpaceScoped p q)

-- | Doesn't fail when existing - just re-assigns
addTimeSpaceForce :: UI.TimeSpace -> Mappings -> Effect Unit
addTimeSpaceForce x@(UI.TimeSpace { id }) (Mappings { timeSpaces }) = addTimeSpaceForceScoped x (S.writeOnly timeSpaces)

-- | Looks for an already flat time space in the sets
getTimeSpace :: TimeSpaceID -> MappingsM SynthesizeError UI.TimeSpace
getTimeSpace = asMappingsM (S.readOnly <<< getTimeSpacesMapping) getTimeSpaceScoped

-- | Includes an already flat timeline - doesn't verify constituents
addTimeline :: UI.Timeline -> MappingsM PopulateError Unit
addTimeline = asMappingsM (S.writeOnly <<< getTimelinesMapping) (\p q -> maybeToEither <$> addTimelineScoped p q)

addTimelineForce :: UI.Timeline -> Mappings -> Effect Unit
addTimelineForce x@(UI.Timeline { id }) (Mappings { timelines }) = IxSignalMap.assign id x timelines

-- | Looks for an already flat timeline in the sets
getTimeline :: TimelineID -> MappingsM SynthesizeError UI.Timeline
getTimeline = asMappingsM (S.readOnly <<< getTimelinesMapping) getTimelineScoped

-- | Includes an already flat event as a sibling - doesn't verify constituents
addEvent :: UI.Event -> MappingsM PopulateError Unit
addEvent = asMappingsM (S.writeOnly <<< getEventsMapping) (\p q -> maybeToEither <$> addEventScoped p q)

addEventForce :: UI.Event -> Mappings -> Effect Unit
addEventForce x@(UI.Event { id }) (Mappings { events }) = IxSignalMap.assign id x events

-- | Looks for an already flat event (as a sibling) in the sets
getEvent :: EventID -> MappingsM SynthesizeError UI.Event
getEvent = asMappingsM (S.readOnly <<< getEventsMapping) getEventScoped

-- | Includes an already flat time span as a sibling - doesn't verify constituents
addTimeSpan :: UI.TimeSpan -> MappingsM PopulateError Unit
addTimeSpan = asMappingsM (S.writeOnly <<< getTimeSpansMapping) (\p q -> maybeToEither <$> addTimeSpanScoped p q)

addTimeSpanForce :: UI.TimeSpan -> Mappings -> Effect Unit
addTimeSpanForce x@(UI.TimeSpan { id }) (Mappings { timeSpans }) = IxSignalMap.assign id x timeSpans

-- | Looks for an already flat time span (as a sibling) in the sets
getTimeSpan :: TimeSpanID -> MappingsM SynthesizeError UI.TimeSpan
getTimeSpan = asMappingsM (S.readOnly <<< getTimeSpansMapping) getTimeSpanScoped

getEventOrTimeSpan :: UI.EventOrTimeSpanPoly EventID TimeSpanID -> MappingsM SynthesizeError UI.EventOrTimeSpan
getEventOrTimeSpan = asMappingsM getEventsAndTimeSpansMappings getEventOrTimeSpanAsTuple
  where
  getEventsAndTimeSpansMappings uiSets = Tuple (S.readOnly (getEventsMapping uiSets)) (S.readOnly (getTimeSpansMapping uiSets))

  getEventOrTimeSpanAsTuple id (Tuple events timeSpans) = getEventOrTimeSpanScoped id events timeSpans

-- | Assigns the root field of a set
setRoot :: TimeSpaceID -> Mappings -> Effect Unit
setRoot id (Mappings { root }) = Ref.write (Just id) root

getRoot :: Mappings -> Effect (Maybe TimeSpaceID)
getRoot (Mappings { root }) = Ref.read root

-- | Helper for shmoozing types to work with MappingsM
maybeToEither :: forall e. Maybe e -> Either e Unit
maybeToEither mX = case mX of
  Nothing -> Right unit
  Just e -> Left e
