{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Golden test management, interactive mode. Runs the tests, and asks
-- the user how to proceed in case of failure or missing golden standard.
module Test.Tasty.Silver.Interactive
  (
  -- * Command line helpers
    defaultMain
  , defaultMain1

  -- * The ingredient
  , interactiveTests
  , Interactive (..)

  -- * Programmatic API
  , runTestsInteractive
  )
  where

import Prelude hiding (fail)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Silver.Filter
import Test.Tasty.Silver.Internal
import Test.Tasty.Silver.Interactive.Run
import Data.Typeable
import Data.Tagged
import Data.Maybe
import Data.Monoid
import qualified Data.Text.IO as TIO
#if __GLASGOW_HASKELL__ < 708
import Data.Foldable (foldMap)
#endif
import Data.Char
import qualified Data.IntMap as IntMap
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Control.Monad.State hiding (fail)
import Control.Monad.STM
import Control.Monad.Reader hiding (fail)
import Control.Monad.Identity hiding (fail)
import Control.Concurrent.STM.TVar
import Control.Exception
import Text.Printf
import qualified Data.Text as T
import Data.Text.Encoding
import Options.Applicative hiding (Failure, Success)
import System.Process.ByteString as PS
import System.Process
import qualified Data.ByteString as BS
import System.Directory
import System.IO
import System.IO.Temp
import System.FilePath
import Test.Tasty.Providers
import System.Console.ANSI
import qualified System.Process.Text as PTL

type DisabledTests = TestPath -> Bool

-- | Like @defaultMain@ from the main tasty package, but also includes the
-- golden test management capabilities.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMain1 []


defaultMain1 :: ([RegexFilter]) -> TestTree -> IO ()
defaultMain1 filters = defaultMainWithIngredients
        [ listingTests
        , interactiveTests (checkRF False filters)
        ]

newtype Interactive = Interactive Bool
  deriving (Eq, Ord, Typeable)
instance IsOption Interactive where
  defaultValue = Interactive False
  parseValue = fmap Interactive . safeRead
  optionName = return "interactive"
  optionHelp = return "Run tests in interactive mode."
  optionCLParser = flagCLParser (Just 'i') (Interactive True)

data ResultType = RTSuccess | RTFail | RTIgnore
  deriving (Eq)

data FancyTestException
  = Mismatch GoldenResultI
  | Disabled
  deriving (Show, Typeable)

instance Exception FancyTestException

getResultType :: Result -> ResultType
getResultType (Result { resultOutcome = Success}) = RTSuccess
getResultType (Result { resultOutcome = (Failure (TestThrewException e))}) =
  case fromException e of
    Just Disabled -> RTIgnore
    _ -> RTFail
getResultType (Result { resultOutcome = (Failure _)}) = RTFail


interactiveTests :: DisabledTests
    -> Ingredient
interactiveTests dis = TestManager
    [ Option (Proxy :: Proxy Interactive)
    , Option (Proxy :: Proxy HideSuccesses)
    , Option (Proxy :: Proxy UseColor)
    , Option (Proxy :: Proxy NumThreads)
    , Option (Proxy :: Proxy ExcludeFilters)
    , Option (Proxy :: Proxy IncludeFilters)
    ] $
  \opts tree ->
      Just $ runTestsInteractive dis opts (filterWithRegex opts tree)

runSingleTest ::  IsTest t => DisabledTests -> TestPath -> TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result
runSingleTest dis tp _ _ _ _ | dis tp = 
  return $ (testFailed "")
    { resultOutcome = (Failure $ TestThrewException $ toException Disabled) }
runSingleTest _ _ _ opts t cb = do
  case (cast t :: Maybe Golden) of
    Nothing -> run opts t cb
    Just g -> do
        (r, gr) <- runGolden g

        -- we may be in a different thread here than the main ui.
        -- force evaluation of actual value here, as we have to evaluate it before
        -- leaving this test.
        gr' <- forceGoldenResult gr
        case gr' of
            GREqual -> return r
            grd -> return $ r { resultOutcome = (Failure $ TestThrewException $ toException $ Mismatch grd) }

-- | A simple console UI
runTestsInteractive :: DisabledTests -> OptionSet -> TestTree -> IO Bool
runTestsInteractive dis opts tests = do
  let tests' = wrapRunTest (runSingleTest dis) tests

  r <- launchTestTree opts tests' $ \smap ->
    do
    isTerm <- hSupportsANSI stdout

    (\k -> if isTerm
      then (do hideCursor; k) `finally` showCursor
      else k) $ do

      hSetBuffering stdout NoBuffering

      let
        whenColor = lookupOption opts
        HideSuccesses hideSuccesses = lookupOption opts

      let
        ?colors = useColor whenColor isTerm

      let
        outp = produceOutput opts tests

      stats <- case () of { _
        | hideSuccesses && isTerm ->
            consoleOutputHidingSuccesses outp smap
        | hideSuccesses && not isTerm ->
            streamOutputHidingSuccesses outp smap
        | otherwise -> consoleOutput outp smap
      }

      return $ \time -> do
            printStatistics stats time
            return $ statFailures stats == 0



  return r


printDiff :: TestName -> GDiff -> IO ()
printDiff n (DiffText _ tGold tAct) = withDiffEnv n tGold tAct
  (\fGold fAct -> do
        (_, stdOut, _) <- PTL.readProcessWithExitCode "sh" ["-c", "git diff --no-index --text " ++ fGold ++ " " ++ fAct] T.empty
        TIO.putStrLn stdOut

  )
printDiff _ (ShowDiffed _ t) = TIO.putStrLn t
printDiff _ Equal = error "Can't print diff for equal values."

showDiff :: TestName -> GDiff -> IO ()
showDiff n (DiffText _ tGold tAct) = do
  hasColorDiff' <- hasColorDiff

  withDiffEnv n tGold tAct
    (if hasColorDiff' then colorDiff else gitDiff)
  where
    gitDiff fGold fAct = callProcess "sh"
        ["-c", "git diff --color=always --no-index --text " ++ fGold ++ " " ++ fAct ++ " | less -r > /dev/tty"]

    doesCmdExist cmd = isJust <$> findExecutable cmd

    hasColorDiff = (&&) <$> doesCmdExist "wdiff" <*> doesCmdExist "colordiff"

    colorDiff fGold fAct = callProcess "sh" ["-c", "wdiff " ++ fGold ++ " " ++ fAct ++ " | colordiff | less -r > /dev/tty"]
showDiff n (ShowDiffed _ t) = showInLess n t
showDiff _ Equal = error "Can't show diff for equal values."

-- Stores the golden/actual text in two files, so we can use it for git diff.
withDiffEnv :: TestName -> T.Text -> T.Text -> (FilePath -> FilePath -> IO ()) -> IO ()
withDiffEnv n tGold tAct cont = do
  withSystemTempFile (n <.> "golden") (\fGold hGold -> do
    withSystemTempFile (n <.> "actual") (\fAct hAct -> do
      hSetBinaryMode hGold True
      hSetBinaryMode hAct True
      BS.hPut hGold (encodeUtf8 tGold)
      BS.hPut hAct (encodeUtf8 tAct)
      hClose hGold
      hClose hAct
      cont fGold fAct
      )
    )


printValue :: TestName -> GShow -> IO ()
printValue _ (ShowText t) = TIO.putStrLn t

showValue :: TestName -> GShow -> IO ()
showValue n (ShowText t) = showInLess n t

showInLess :: String -> T.Text -> IO ()
showInLess _ t = do
  -- TODO error handling...
  _ <- PS.readProcessWithExitCode "sh" ["-c", "less > /dev/tty"] inp
  return ()
  where inp = encodeUtf8 t

tryAccept :: String -> TestName -> (a -> IO ()) -> a -> IO Bool
tryAccept pref nm upd new = do
  isTerm <- hSupportsANSI stdout
  when isTerm showCursor
  _ <- printf "%sAccept actual value as new golden value? [yn] " pref
  ans <- getLine
  case ans of
    "y" -> do
        upd new
        when isTerm hideCursor
        printf "%s" pref
        return True
    "n" -> do
        printf "%s" pref
        when isTerm hideCursor
        return False
    _   -> do
        printf "%sInvalid answer.\n" pref
        tryAccept pref nm upd new


--------------------------------------------------
-- TestOutput base definitions
--------------------------------------------------
-- {{{
-- | 'TestOutput' is an intermediary between output formatting and output
-- printing. It lets us have several different printing modes (normal; print
-- failures only; quiet).
data TestOutput
  = HandleTest
      {- test name, used for golden lookup #-} (TestName)
      {- print test name   -} (IO ())
      {- print test result -} (Result -> IO Statistics)
  | PrintHeading (IO ()) TestOutput
  | Skip
  | Seq TestOutput TestOutput

-- The monoid laws should hold observationally w.r.t. the semantics defined
-- in this module
instance Monoid TestOutput where
  mempty = Skip
  mappend = Seq

type Level = Int

produceOutput :: (?colors :: Bool) => OptionSet -> TestTree -> TestOutput
produceOutput opts tree =
  let
    -- Do not retain the reference to the tree more than necessary
    !alignment = computeAlignment opts tree
    Interactive isInteractive = lookupOption opts

    handleSingleTest
      :: (IsTest t, ?colors :: Bool)
      => OptionSet -> TestName -> t -> Ap (Reader Level) TestOutput
    handleSingleTest _opts name _test = Ap $ do
      level <- ask

      let
        align = replicate (alignment - indentSize * level - length name) ' '
        pref = indent level ++ replicate (length name) ' ' ++ "  " ++ align
        printTestName =
          printf "%s%s: %s" (indent level) name align
        printResultLine result forceTime = do
          -- use an appropriate printing function
          let
            resTy = getResultType result
            printFn = case resTy of
                RTSuccess -> ok
                RTIgnore -> warn
                RTFail -> fail
          case resTy of
            RTSuccess -> printFn "OK"
            RTIgnore -> printFn "DISABLED"
            RTFail -> printFn "FAIL"
          -- print time only if it's significant
          when (resultTime result >= 0.01 || forceTime) $
            printFn (printf " (%.2fs)" $ resultTime result)
          printFn "\n"


        handleTestResult result = do
          -- non-interactive mode. Uses different order of printing,
          -- as using the interactive layout doesn't go that well
          -- with printing the diffs to stdout.
          --
          printResultLine result True

          rDesc <- formatMessage $ resultDescription result
          when (not $ null rDesc) $ (case getResultType result of
            RTSuccess -> infoOk
            RTIgnore -> infoWarn
            RTFail -> infoFail) $
              printf "%s%s\n" pref (formatDesc (level+1) rDesc)

          stat' <- printTestOutput pref name result

          return stat'

        handleTestResultInteractive result = do
          (result', stat') <- case (resultOutcome result) of
            Failure (TestThrewException e) ->
              case fromException e of
                Just (Mismatch (GRDifferent _ a diff Nothing)) -> do
                  printResultLine result False
                  s <- printTestOutput pref name result
                  return (testFailed "", s)
                Just (Mismatch (GRNoGolden a shw (Just upd))) -> do
                  printf "Golden value missing. Press <enter> to show actual value.\n"
                  _ <- getLine
                  let a' = runIdentity a
                  shw' <- shw a'
                  showValue name shw'
                  isUpd <- tryAccept pref name upd a'
                  let r =
                        if isUpd
                        then ( testPassed "Created golden value."
                             , mempty { statCreatedGolden = 1 } )
                        else ( testFailed "Golden value missing."
                             , mempty { statFailures = 1 } )
                  printResultLine (fst r) False
                  return r
                Just (Mismatch (GRDifferent _ a diff (Just upd))) -> do
                  printf "Golden value differs from actual value.\n"
                  showDiff name diff
                  isUpd <- tryAccept pref name upd a
                  let r =
                        if isUpd
                        then ( testPassed "Updated golden value."
                             , mempty { statUpdatedGolden = 1 } )
                        else ( testFailed "Golden value does not match actual output."
                             , mempty { statFailures = 1 } )
                  printResultLine (fst r) False
                  return r
                Just (Mismatch _) -> error "Impossible case!"
                Just Disabled -> do
                  printResultLine result False
                  return ( result
                         , mempty { statDisabled = 1 } )
                Nothing -> do
                  printResultLine result False
                  return (result, mempty {statFailures = 1})
            Success -> do
              printResultLine result False
              return (result, mempty { statSuccesses = 1 })
            Failure _ -> do
              printResultLine result False
              return (result, mempty { statFailures = 1 })

          let result'' = result' { resultTime = resultTime result }

          rDesc <- formatMessage $ resultDescription result''
          when (not $ null rDesc) $ (case getResultType result'' of
            RTSuccess -> infoOk
            RTIgnore -> infoWarn
            RTFail -> infoFail) $
              printf "%s%s\n" pref (formatDesc (level+1) rDesc)

          return stat'

      let handleTestResult' = (if isInteractive then handleTestResultInteractive else handleTestResult)
      return $ HandleTest name printTestName handleTestResult'

    handleGroup :: TestName -> Ap (Reader Level) TestOutput -> Ap (Reader Level) TestOutput
    handleGroup name grp = Ap $ do
      level <- ask
      let
        printHeading = printf "%s%s\n" (indent level) name
        printBody = runReader (getApp grp) (level + 1)
      return $ PrintHeading printHeading printBody

  in
    flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = handleSingleTest
          , foldGroup = handleGroup
          }
          opts tree

printTestOutput :: (?colors :: Bool) => String -> TestName -> Result -> IO Statistics
printTestOutput prefix name result = case resultOutcome result of
  Failure (TestThrewException e) ->
    case fromException e of
      Just (Mismatch (GRNoGolden a shw _)) -> do
        infoFail $ printf "%sActual value is:\n" prefix
        let a' = runIdentity a
        shw' <- shw a'
        hsep
        printValue name shw'
        hsep
        return ( mempty { statFailures = 1 } )
      Just (Mismatch (GRDifferent _ _ diff _)) -> do
        infoFail $ printf "%sDiff between actual and golden value:\n" prefix
        hsep
        printDiff name diff
        hsep
        return ( mempty { statFailures = 1 } )
      Just (Mismatch _) -> error "Impossible case!"
      Just Disabled -> return ( mempty { statDisabled = 1 } )
      Nothing -> return ( mempty { statFailures = 1 } )
  Failure _ -> return ( mempty { statFailures = 1 } )
  Success -> return ( mempty { statSuccesses = 1 } )

hsep :: IO ()
hsep = putStrLn (replicate 40 '=')

foldTestOutput
  :: (?colors :: Bool, Monoid b)
  => (IO () -> IO Result
    -> (Result -> IO Statistics)
    -> b)
  -> (IO () -> b -> b)
  -> TestOutput -> StatusMap -> b
foldTestOutput foldTest foldHeading outputTree smap =
  flip evalState 0 $ getApp $ go outputTree where
  go (HandleTest _ printName handleResult) = Ap $ do
    ix <- get
    put $! ix + 1
    let
      statusVar =
        fromMaybe (error "internal error: index out of bounds") $
        IntMap.lookup ix smap
      readStatusVar = getResultFromTVar statusVar
    return $ foldTest printName readStatusVar handleResult
  go (PrintHeading printName printBody) = Ap $
    foldHeading printName <$> getApp (go printBody)
  go (Seq a b) = mappend (go a) (go b)
  go Skip = mempty

-- }}}

--------------------------------------------------
-- TestOutput modes
--------------------------------------------------
-- {{{
consoleOutput :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
consoleOutput outp smap =
  getApp . fst $ foldTestOutput foldTest foldHeading outp smap
  where
    foldTest printName getResult handleResult =
      (Ap $ do
        _ <- printName
        r <- getResult
        handleResult r
      , Any True)
    foldHeading printHeading (printBody, Any nonempty) =
      (Ap $ do
        when nonempty $ printHeading
        stats <- getApp printBody
        return stats
      , Any nonempty )

consoleOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
consoleOutputHidingSuccesses outp smap =
  snd <$> (getApp $ foldTestOutput foldTest foldHeading outp smap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          _ <- printName
          r <- getResult
          if resultSuccessful r
            then do
                clearThisLine
                return (Any False, mempty { statSuccesses = 1 })
            else do
                stats <- handleResult r
                return (Any True, stats)

    foldHeading printHeading printBody =
      Ap $ do
        _ <- printHeading
        b@(Any failed, _) <- getApp printBody
        unless failed clearAboveLine
        return b

    clearAboveLine = do cursorUpLine 1; clearThisLine
    clearThisLine = do clearLine; setCursorColumn 0

streamOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
streamOutputHidingSuccesses outp smap =
  snd <$> (flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading outp smap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful r
            then return (Any False, mempty { statSuccesses = 1 })
            else do
              stack <- get
              put []

              stats <- liftIO $ do
                sequence_ $ reverse stack
                _ <- printName
                handleResult r

              return (Any True, stats)

    foldHeading printHeading printBody =
      Ap $ do
        modify (printHeading :)
        b@(Any failed, _) <- getApp printBody
        unless failed $
          modify $ \stack ->
            case stack of
              _:rest -> rest
              [] -> [] -- shouldn't happen anyway
        return b

-- }}}

--------------------------------------------------
-- Statistics
--------------------------------------------------
-- {{{

data Statistics = Statistics
  { statSuccesses :: !Int
  , statUpdatedGolden :: !Int
  , statCreatedGolden :: !Int
  , statFailures :: !Int
  , statDisabled :: !Int
  }

instance Monoid Statistics where
  Statistics a1 b1 c1 d1 e1 `mappend` Statistics a2 b2 c2 d2 e2 = Statistics (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)
  mempty = Statistics 0 0 0 0 0

printStatistics :: (?colors :: Bool) => Statistics -> Time -> IO ()
printStatistics st time = do
  printf "\n"

  let total = statFailures st + statUpdatedGolden st + statCreatedGolden st + statSuccesses st

  when (statCreatedGolden st > 0) (printf "Created %d golden values.\n" (statCreatedGolden st))
  when (statUpdatedGolden st > 0) (printf "Updated %d golden values.\n" (statUpdatedGolden st))
  when (statDisabled st > 0) (printf "Ignored %d disabled tests.\n" (statDisabled st))

  case statFailures st of
    0 -> do
      ok $ printf "All %d tests passed (%.2fs)\n" total time

    fs -> do
      fail $ printf "%d out of %d tests failed (%.2fs)\n" fs total time

data FailureStatus
  = Unknown
  | Failed
  | OK

instance Monoid FailureStatus where
  mappend Failed _ = Failed
  mappend _ Failed = Failed

  mappend OK OK = OK

  mappend _ _ = Unknown

  mempty = OK

-- }}}

--------------------------------------------------
-- Console test reporter
--------------------------------------------------

-- | Report only failed tests
newtype HideSuccesses = HideSuccesses Bool
  deriving (Eq, Ord, Typeable)
instance IsOption HideSuccesses where
  defaultValue = HideSuccesses False
  parseValue = fmap HideSuccesses . safeRead
  optionName = return "hide-successes"
  optionHelp = return "Do not print tests that passed successfully"
  optionCLParser = flagCLParser Nothing (HideSuccesses True)

-- | When to use color on the output
data UseColor
  = Never | Always | Auto
  deriving (Eq, Ord, Typeable)

-- | Control color output
instance IsOption UseColor where
  defaultValue = Auto
  parseValue = parseUseColor
  optionName = return "color"
  optionHelp = return "When to use colored output. Options are 'never', 'always' and 'auto' (default: 'auto')"
  optionCLParser =
    option parse
      (  long name
      <> help (untag (optionHelp :: Tagged UseColor String))
      )
    where
      name = untag (optionName :: Tagged UseColor String)
      parse = str >>=
        maybe (readerError $ "Could not parse " ++ name) pure <$> parseValue

-- | @useColor when isTerm@ decides if colors should be used,
--   where @isTerm@ denotes where @stdout@ is a terminal device.
useColor :: UseColor -> Bool -> Bool
useColor cond isTerm =
  case cond of
    Never  -> False
    Always -> True
    Auto   -> isTerm

parseUseColor :: String -> Maybe UseColor
parseUseColor s =
  case map toLower s of
    "never"  -> return Never
    "always" -> return Always
    "auto"   -> return Auto
    _        -> Nothing

-- }}}

--------------------------------------------------
-- Various utilities
--------------------------------------------------
-- {{{

{-getResultWithGolden :: StatusMap -> GoldenStatusMap -> TestName -> Int -> IO (Result, ResultStatus)
getResultWithGolden smap gmap nm ix = do
  r <- getResultFromTVar statusVar

  gr <- atomically $ readTVar gmap
  case nm `M.lookup` gr of
    Just g@(GRDifferent {}) -> return (r, RMismatch g)
    Just g@(GRNoGolden {})  -> return (r, RMismatch g)
    _ | resultSuccessful r  -> return (r, RPass)
    _ | resultOutcome r
    _ | otherwise           -> return (r, RFail)
  where statusVar =
            fromMaybe (error "internal error: index out of bounds") $
            IntMap.lookup ix smap
-}

getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar statusVar = do
  atomically $ do
    status <- readTVar statusVar
    case status of
      Done r -> return r
      _ -> retry



-- }}}

--------------------------------------------------
-- Formatting
--------------------------------------------------
-- {{{

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

-- handle multi-line result descriptions properly
formatDesc
  :: Int -- indent
  -> String
  -> String
formatDesc n desc =
  let
    -- remove all trailing linebreaks
    chomped = reverse . dropWhile (== '\n') . reverse $ desc

    multiline = '\n' `elem` chomped

    -- we add a leading linebreak to the description, to start it on a new
    -- line and add an indentation
    paddedDesc = flip concatMap chomped $ \c ->
      if c == '\n'
        then c : indent n
        else [c]
  in
    if multiline
      then paddedDesc
      else chomped

data Maximum a
  = Maximum a
  | MinusInfinity

instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity

  Maximum a `mappend` Maximum b = Maximum (a `max` b)
  MinusInfinity `mappend` a = a
  a `mappend` MinusInfinity = a

-- | Compute the amount of space needed to align "OK"s and "FAIL"s
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid .
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ level -> Maximum (length name + level)
      , foldGroup = \_ m -> m . (+ indentSize)
      }
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- (Potentially) colorful output
ok, warn, fail, infoOk, infoWarn, infoFail :: (?colors :: Bool) => String -> IO ()
ok       = output NormalIntensity Dull  Green
warn     = output NormalIntensity Dull  Yellow
fail     = output BoldIntensity   Vivid Red
infoOk   = output NormalIntensity Dull  White
infoWarn = output NormalIntensity Dull  White
infoFail = output NormalIntensity Dull  Red

output
  :: (?colors :: Bool)
  => ConsoleIntensity
  -> ColorIntensity
  -> Color
  -> String
  -> IO ()
output bold intensity color st
  | ?colors =
    (do
      setSGR
        [ SetColor Foreground intensity color
        , SetConsoleIntensity bold
        ]
      putStr st
    ) `finally` setSGR []
  | otherwise = putStr st

-- }}}
