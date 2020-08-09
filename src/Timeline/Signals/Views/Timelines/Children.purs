module Timeline.Signals.Views.Timelines.Children where

-- FIXME mimic Siblings, and figure out the caching stuff

-- newtype Children
--   = Children (Array EventOrTimeSpan)

-- derive instance genericChildren :: Generic Children _

-- derive newtype instance eqChildren :: Eq Children

-- derive newtype instance showChildren :: Show Children

-- derive newtype instance encodeJsonChildren :: EncodeJson Children

-- derive newtype instance decodeJsonChildren :: DecodeJson Children

-- derive newtype instance arbitraryChildren :: Arbitrary Children

-- -- FIXME dummy data
-- instance defaultChildren :: Default Children where
--   def =
--     let
--       renameEvent s v =
--         let
--           Event x = def
--         in
--           Event x { name = s, time = v }

--       renameTimeSpan s v =
--         let
--           TimeSpan x = def
--         in
--           TimeSpan x { name = s, span = v }
--     in
--       Children
--         [ EventOrTimeSpan $ Left (renameEvent "Event A" (DecidedValueNumber 3.0))
--         , EventOrTimeSpan $ Left (renameEvent "Event B" (DecidedValueNumber 3.5))
--         , EventOrTimeSpan $ Right (renameTimeSpan "TimeSpan C" (DecidedSpanNumber { start: 2.0, stop: 5.0 }))
--         , EventOrTimeSpan $ Right (renameTimeSpan "TimeSpan D" (DecidedSpanNumber { start: 1.0, stop: 4.0 }))
--         ]

-- localstorageSignalKey :: String
-- localstorageSignalKey = "localstorage"

-- localstorageKey :: String
-- localstorageKey = "Children"

-- -- TODO predicate from top-level index, and seek from selected time space.
-- newChildrenSignal ::
--   { settingsSignal :: IxSignal ( read :: S.READ ) Settings
--   , initialChildren :: Maybe Children
--   } ->
--   Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) (Maybe Children))
-- newChildrenSignal { settingsSignal, initialChildren } = do
--   store <- window >>= localStorage
--   mItem <- getItem localstorageKey store
--   item <- case mItem of
--     Nothing -> pure initialChildren
--     Just s -> case parseJson s >>= decodeJson of
--       Left e -> throw $ "Couldn't parse Children: " <> show e
--       Right x -> pure (Just x)
--   sig <- make item
--   let
--     handler x = do
--       Settings { localCacheTilExport } <- get settingsSignal
--       when localCacheTilExport
--         $ setItem localstorageKey (stringify (encodeJson x)) store
--   subscribeDiffLight localstorageSignalKey handler sig
--   pure sig

-- clearChildrenCache :: Effect Unit
-- clearChildrenCache = do
--   store <- window >>= localStorage
--   removeItem localstorageKey store

-- setNewDocumentChildren ::
--   IxSignal ( write :: S.WRITE ) (Maybe Children) ->
--   Effect Unit
-- setNewDocumentChildren childrenSignal = set Nothing childrenSignal
