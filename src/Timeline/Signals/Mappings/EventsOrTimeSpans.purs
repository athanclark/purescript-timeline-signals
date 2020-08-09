module Timeline.Signals.Mappings.EventsOrTimeSpans where

import Timeline.Signals.Mappings.Events (getEventScoped)
import Timeline.Signals.Mappings.TimeSpans (getTimeSpanScoped)
import Timeline.Signals.Mappings.Errors (SynthesizeError)
import Timeline.UI.TimeSpan (TimeSpan)
import Timeline.UI.Event (Event)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan(..), EventOrTimeSpanPoly(..))
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.ID.Event (EventID)
import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Zeta.Types (READ) as S
import IxZeta.Map (IxSignalMap)

getEventOrTimeSpanScoped ::
  EventOrTimeSpanPoly EventID TimeSpanID ->
  IxSignalMap EventID ( read :: S.READ ) Event ->
  IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan ->
  Effect (Either SynthesizeError EventOrTimeSpan)
getEventOrTimeSpanScoped (EventOrTimeSpanPoly eOrTs) events timeSpans = case eOrTs of
  Left e -> map (EventOrTimeSpan <<< Left) <$> getEventScoped e events
  Right ts -> map (EventOrTimeSpan <<< Right) <$> getTimeSpanScoped ts timeSpans
