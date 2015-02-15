{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Silver.Internal where

import Control.Applicative
import Control.Exception
import Data.Typeable (Typeable)
import Data.ByteString as SB
import System.IO.Error
import qualified Data.Text as T
import Options.Applicative
import Data.Tagged
import Data.Proxy
import Test.Tasty.Providers
import Test.Tasty.Options

-- | See 'goldenTest1' for explanation of the fields
data Golden =
  forall a .
    Golden
        (IO (Maybe a))    -- Get golden value.
        (IO a)            -- Get actual value.
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

-- | Read the file if it exists, else return Nothing.
-- Useful for reading golden files.
readFileMaybe :: FilePath -> IO (Maybe SB.ByteString)
readFileMaybe path = catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
    (Just <$> SB.readFile path)
    (const $ return Nothing)


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
  ref' <- getGolden
  case ref' of
    Nothing | accept -> do
          new <- getActual
          upd new
          return $ testPassed "Created golden file."
    Nothing -> return $ testFailed "Missing golden value."
    Just ref -> do
      new <- getActual
      -- Output could be arbitrarily big, so don't even try to say what wen't wrong.
      cmp' <- cmp ref new
      case cmp' of
        Equal -> return $ testPassed ""
        _ | accept -> do
              upd new
              return $ testPassed "Updated golden file."
        _     -> return $ testFailed "Result did not match expected output. Use interactive mode to see the full output."
