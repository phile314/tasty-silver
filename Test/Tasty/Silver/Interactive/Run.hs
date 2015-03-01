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


-- | Provide new test run function wrapping the existing tests.
wrapRunTest :: (forall t . IsTest t => TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result) -> TestTree -> TestTree
wrapRunTest f (SingleTest n t) = SingleTest n (CustomTestExec t (f n))
wrapRunTest f (TestGroup n ts) = TestGroup n (fmap (wrapRunTest f) ts)
wrapRunTest f (PlusTestOptions o t) = PlusTestOptions o (wrapRunTest f t)
wrapRunTest f (WithResource r t) = WithResource r (\x -> wrapRunTest f (t x))
wrapRunTest f (AskOptions t) = AskOptions (\o -> wrapRunTest f (t o))
