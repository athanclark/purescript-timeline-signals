module Test.Main where

-- import Timeline.Signals.Mappings (Mappings (..))
-- import Timeline.Signals.Mappings.TimeSpaces (addTimeSpaceForceScoped)
-- import Timeline.Signals.Mappings.Timelines (addTimelineScoped)
-- import Timeline.Signals.Mappings.Events (addEventScoped)
-- import Timeline.Signals.Mappings.TimeSpans (addTimeSpanScoped)
-- import Timeline.UI.TimeSpace (TimeSpace (..))
-- import Timeline.UI.Timeline (Timeline)
-- import Timeline.UI.Event (Event)
-- import Timeline.UI.TimeSpan (TimeSpan)

import Prelude
-- import Data.Maybe (Maybe (..))
import Data.Identity (Identity)
-- import Data.NonEmpty (NonEmpty (..))
-- import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
-- import Effect.Ref (new) as Ref
-- import Effect.Unsafe (unsafePerformEffect)
-- import Zeta.Types (writeOnly) as Z
-- import IxZeta.Map (new) as IxSignalMap
-- import Unsafe.Coerce (unsafeCoerce)
import Test.Spec (describe, SpecT)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
-- import Test.QuickCheck (arbitrary)
-- import Test.QuickCheck.Gen (Gen)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] tests

tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Mapping Tests" do
    pure unit

-- genMappings :: Gen Mappings
-- genMappings = do
--   NonEmpty rootTimeSpace@(TimeSpace {id: rootID}) (timeSpaces' :: Array TimeSpace) <- arbitrary
--   (timelines' :: Array Timeline) <- arbitrary
--   (events' :: Array Event) <- arbitrary
--   (timeSpans' :: Array TimeSpan) <- arbitrary -- FIXME interlink generated entities
--   let root = unsafePerformEffect (Ref.new (Just rootID))
--       timeSpaces = unsafePerformEffect do
--         mapping <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
--         addTimeSpaceForceScoped rootTimeSpace (Z.writeOnly mapping)
--         traverse_ (\x -> addTimeSpaceForceScoped x (Z.writeOnly mapping)) timeSpaces'
--         pure mapping
--       timelines = unsafePerformEffect do
--         mapping <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
--         traverse_ (\x -> void (addTimelineScoped x (Z.writeOnly mapping))) timelines'
--         pure mapping
--       events = unsafePerformEffect do
--         mapping <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
--         traverse_ (\x -> void (addEventScoped x (Z.writeOnly mapping))) events'
--         pure mapping
--       timeSpans = unsafePerformEffect do
--         mapping <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
--         traverse_ (\x -> void (addTimeSpanScoped x (Z.writeOnly mapping))) timeSpans'
--         pure mapping
--   pure $ Mappings {root, timeSpaces, timelines, events, timeSpans}
