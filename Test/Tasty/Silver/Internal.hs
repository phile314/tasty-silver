{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Silver.Internal where

import Control.Applicative
import Control.Monad.Cont
import Control.Exception
import Data.Typeable (Typeable)
import Data.ByteString.Lazy as LB
import System.IO
import System.IO.Error
import qualified Data.Text as T
import Data.Maybe
import Options.Applicative
import Data.Tagged
import Data.Proxy
import Test.Tasty.Providers
import Test.Tasty.Options

-- | See 'goldenTest1' for explanation of the fields
data Golden =
  forall a .
    Golden
        (forall r . ValueGetter r (Maybe a))    -- Get golden value.
        (forall r . ValueGetter r a)            -- Get actual value.
        (a -> a -> IO GDiff)                       -- Compare/diff.
        (a -> IO GShow)                            -- How to produce a show.
        (a -> IO ())                            -- Update golden value.
  deriving Typeable


-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode
newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord, Typeable)
instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeRead
  optionName = return "accept"
  optionHelp = return "Accept current results of golden tests"
  optionCLParser =
    fmap AcceptTests $
    switch
      (  long (untag (optionName :: Tagged AcceptTests String))
      <> help (untag (optionHelp :: Tagged AcceptTests String))
      )

-- | An action that yields a value (either golden or tested).
--
-- CPS allows closing the file handle when using lazy IO to read data.
newtype ValueGetter r a = ValueGetter
  { runValueGetter :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: FilePath -> ValueGetter r ByteString
vgReadFile path = fromJust <$> vgReadFile1 predFalse path
  where predFalse :: IOException -> Bool
        predFalse _ = False

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
-- Will return 'Nothing' if the file does not exist.
vgReadFileMaybe :: FilePath -> ValueGetter r (Maybe ByteString)
vgReadFileMaybe = vgReadFile1 (isDoesNotExistErrorType . ioeGetErrorType)


-- | Reads a file, and optionally catches some exceptions. If
-- an exception is catched, Nothing is returned.
vgReadFile1 :: Exception e
    => (e -> Bool)  -- ^ Which exceptions to catch.
    -> FilePath
    -> ValueGetter r (Maybe ByteString)
vgReadFile1 doCatch path = do
  r <- ValueGetter $
    ContT $ \k ->
    catchJust (\e -> if doCatch e then Just () else Nothing)
      (bracket
        (openBinaryFile path ReadMode)
        hClose
        (\h -> LB.hGetContents h >>= (k . Just))
      )
      (const $ k Nothing)
  return $! r

-- | Ensures that the result is fully evaluated (so that lazy file handles
-- can be closed)
vgRun :: ValueGetter r r -> IO r
vgRun (ValueGetter a) = runContT a evaluate

-- | The comparison/diff result.
data GDiff
  = Equal -- ^ Values are equal.
  | DiffText { gActual :: T.Text, gExpected :: T.Text } -- ^ The two values are different, show a diff between the two given texts.
  | ShowDiffed { gDiff :: T.Text }  -- ^ The two values are different, just show the given text to the user.

-- | How to show a value to the user.
data GShow
  = ShowText T.Text     -- ^ Show the given text.

instance IsTest Golden where
  run opts golden _ = runGolden golden (lookupOption opts)
  testOptions = return [Option (Proxy :: Proxy AcceptTests)]

runGolden :: Golden -> AcceptTests -> IO Result
runGolden (Golden getGolden getActual cmp _ upd) (AcceptTests accept) = do
  vgRun $ do
    ref' <- getGolden
    case ref' of
      Nothing | accept -> do
            new <- getActual
            liftIO $ upd new
            return $ testPassed "Created golden file."
      Nothing -> return $ testFailed "Missing golden value."
      Just ref -> do
        new <- getActual
        -- Output could be arbitrarily big, so don't even try to say what wen't wrong.
        cmp' <- liftIO $ cmp ref new
        case cmp' of
          Equal -> return $ testPassed ""
          _ | accept -> do
                liftIO (upd new)
                return $ testPassed "Updated golden file."
          _     -> return $ testFailed "Result did not match expected output. Use interactive mode to see the full output."
