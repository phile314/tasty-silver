{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Regex filtering for test trees.
module Test.Tasty.Silver.Filter
  ( filterWithRegex
  , checkRF
  , RegexFilter (..)
  , IncludeFilters (..)
  , ExcludeFilters (..)
  , TestPath
  )
  where

import Prelude hiding (fail)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Data.Tagged
import Data.Typeable
import Data.Maybe
import Data.Monoid
import qualified Data.List as L
import Options.Applicative
import qualified Text.Regex.TDFA.String as RS
import qualified Text.Regex.TDFA as R

type TestPath = String

-- we have to store the regex as String, as there is no Typeable instance
-- for the Regex data type with GHC < 7.8
data RegexFilter
  = RFInclude String -- include tests that match
  | RFExclude String -- exclude tests that match
  deriving (Typeable)

-- | Tests to completely exlucde, treating them
-- like they do not exist.
newtype ExcludeFilters = ExcludeFilters [RegexFilter]
  deriving (Typeable)

-- | Tests to completely include, treating all
-- other tests like they do not exist.
newtype IncludeFilters = IncludeFilters [RegexFilter]
  deriving (Typeable)

instance IsOption ExcludeFilters where
  defaultValue = ExcludeFilters []
  parseValue = fmap ExcludeFilters . parseValue1 RFExclude
  optionName = return "regex-exclude"
  optionHelp = return "Exclude tests matching a regex (experimental)."
  optionCLParser = parseFilter RFExclude ExcludeFilters

instance IsOption IncludeFilters where
  defaultValue = IncludeFilters []
  parseValue = fmap IncludeFilters . parseValue1 RFInclude
  optionName = return "regex-include"
  optionHelp = return "Include only tests matching a regex (experimental)."
  optionCLParser = parseFilter RFInclude IncludeFilters

compileRegex :: String -> Maybe RS.Regex
compileRegex = either (const Nothing) Just . RS.compile R.defaultCompOpt R.defaultExecOpt

parseFilter :: forall v . IsOption v => (String -> RegexFilter) -> ([RegexFilter] -> v) -> Parser v
parseFilter mkRF mkV = mkV <$> some ( option parse ( long name <> help helpString))
  where
    name = untag (optionName :: Tagged v String)
    helpString = untag (optionHelp :: Tagged v String)
    parse = (str >>=
        either (\err -> readerError $ "Could not parse " ++ name ++ ": " ++ err) (\_ -> mkRF <$> str)
        <$> RS.compile R.defaultCompOpt R.defaultExecOpt)

parseValue1 :: (String -> RegexFilter) -> String -> Maybe [RegexFilter]
parseValue1 f x = fmap (const $ [f x]) $ compileRegex x

filterWithRegex :: OptionSet -> TestTree -> TestTree
filterWithRegex opts = filterWithPred (checkRF True $ excRgxs ++ incRgxs)
  where ExcludeFilters excRgxs = lookupOption opts
        IncludeFilters incRgxs = lookupOption opts


-- | Check if the given path should be kept using regex filters.
-- A Tree leaf is retained if the following conditions
-- are met:
-- 1. At least one RFInclude matches.
-- 2. No RFExclude filter matches.
checkRF :: Bool -- ^ If true, ignore 1. condition if no RFInclude is given.
    -> [RegexFilter]
    -> TestPath -> Bool
checkRF ignNoInc rf tp =
  ((null incRgxs && ignNoInc) || any regexMatches incRgxs)
    && (not $ any regexMatches excRgxs)
  where (incRgxs, excRgxs) = L.partition (isInclude) rf
        isInclude (RFInclude _) = True
        isInclude (RFExclude _) = False

        -- | Returns if the regex matches the test path.
        -- Does NOT differentiate between exclude and include
        -- filters!
        regexMatches :: RegexFilter -> Bool
        regexMatches (RFInclude rgx) = R.matchTest (fromJust $ compileRegex rgx) tp
        regexMatches (RFExclude rgx) = R.matchTest (fromJust $ compileRegex rgx) tp


filterWithPred :: (TestPath -> Bool) -> TestTree -> TestTree
filterWithPred prd tree = fromMaybe emptyTest (filter' "/" tree)
  where x <//> y = x ++ "/" ++ y

        filter' :: TestPath -> TestTree -> Maybe TestTree
        filter' pth (SingleTest n t) = if prd (pth <//> n) then Just $ SingleTest n t else Nothing
        filter' pth (TestGroup n ts) = Just $ TestGroup n (catMaybes $ map (filter' $ pth <//> n) ts)
        filter' pth (PlusTestOptions o t) = PlusTestOptions o <$> filter' pth t
        -- we don't know at tree construction time what the tree wrapped inside an AskOptions/WithResource
        -- is going to look like. We always return something, and just return an empty test group
        -- if later on we see that the child subtree was excluded.
        filter' pth (WithResource r t) = Just $ WithResource r (\x -> fromMaybe emptyTest (filter' pth (t x)))
        filter' pth (AskOptions t) = Just $ AskOptions (\o -> fromMaybe emptyTest (filter' pth (t o)))

        emptyTest = testGroup "" []

