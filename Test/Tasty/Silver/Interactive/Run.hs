{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Silver.Interactive.Run
  ( wrapRunTest
  )
  where

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Data.Typeable
import Data.Tagged
import Test.Tasty.Providers

data CustomTestExec t = IsTest t => CustomTestExec t (OptionSet -> t -> (Progress -> IO ()) -> IO Result)
  deriving (Typeable)

instance IsTest t => IsTest (CustomTestExec t) where
  run opts (CustomTestExec t r) cb = r opts t cb
  testOptions = retag $ (testOptions :: Tagged t [OptionDescription])

type TestPath = String

-- | Provide new test run function wrapping the existing tests.
wrapRunTest :: (forall t . IsTest t => TestPath -> TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result)
    -> TestTree
    -> TestTree
wrapRunTest = wrapRunTest' "/"

wrapRunTest' :: TestPath
    -> (forall t . IsTest t => TestPath -> TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result)
    -> TestTree
    -> TestTree
wrapRunTest' tp f (SingleTest n t) = SingleTest n (CustomTestExec t (f (tp <//> n) n))
wrapRunTest' tp f (TestGroup n ts) = TestGroup n (fmap (wrapRunTest' (tp <//> n) f) ts)
wrapRunTest' tp f (PlusTestOptions o t) = PlusTestOptions o (wrapRunTest' tp f t)
wrapRunTest' tp f (WithResource r t) = WithResource r (\x -> wrapRunTest' tp f (t x))
wrapRunTest' tp f (AskOptions t) = AskOptions (\o -> wrapRunTest' tp f (t o))

(<//>) :: TestPath -> TestPath -> TestPath
a <//> b = a ++ "/" ++ b
