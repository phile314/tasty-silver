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
  , filterWithRegex1
  , RegexFilter (..)
  , IncludeFilters (..)
  , ExcludeFilters (..)
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
#if __GLASGOW_HASKELL__ < 708
import Data.Foldable (foldMap)
#endif
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Options.Applicative
import qualified Text.Regex.TDFA.String as RS
import qualified Text.Regex.TDFA as R

-- we have to store the regex as String, as there is no Typeable instance
-- for the Regex data type with GHC < 7.8
data RegexFilter
  = RFInclude String -- include tests that match
  | RFExclude String -- exclude tests that match
  deriving (Typeable)

newtype ExcludeFilters = ExcludeFilters [RegexFilter]
  deriving (Typeable)

newtype IncludeFilters = IncludeFilters [RegexFilter]
  deriving (Typeable)

compileRegex :: String -> Maybe RS.Regex
compileRegex = either (const Nothing) Just . RS.compile R.defaultCompOpt R.defaultExecOpt

parseFilter :: forall v . IsOption v => (String -> RegexFilter) -> ([RegexFilter] -> v) -> Parser v
parseFilter mkRF mkV = mkV <$> many ( option parse ( long name <> help helpString))
  where
    name = untag (optionName :: Tagged v String)
    helpString = untag (optionHelp :: Tagged v String)
    parse = (str >>=
        either (\err -> readerError $ "Could not parse " ++ name ++ ": " ++ err) (\_ -> mkRF <$> str)
        <$> RS.compile R.defaultCompOpt R.defaultExecOpt)

parseValue1 :: (String -> RegexFilter) -> String -> Maybe [RegexFilter]
parseValue1 f x = fmap (const $ [f x]) $ compileRegex x

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


filterWithRegex :: OptionSet -> TestTree -> TestTree
filterWithRegex opts tree = foldl (filterWithRegex1 opts) tree (excRgxs ++ incRgxs)
  where ExcludeFilters excRgxs = lookupOption opts
        IncludeFilters incRgxs = lookupOption opts


filterWithRegex1 :: OptionSet -> TestTree -> RegexFilter -> TestTree
filterWithRegex1 _ tree rf = fromMaybe emptyTest (filter' "/" tree)
  where x <//> y = x ++ "/" ++ y

        prd = case rf of
            RFInclude rgx -> R.matchTest (fromJust $ compileRegex rgx)
            RFExclude rgx -> not . R.matchTest (fromJust $ compileRegex rgx)

        filter' :: String -> TestTree -> Maybe TestTree
        filter' pth (SingleTest n t) = if prd (pth <//> n) then Just $ SingleTest n t else Nothing
        filter' pth (TestGroup n ts) = if prd pth' then Just $ TestGroup n (catMaybes $ map (filter' pth') ts) else Nothing
          where pth' = pth <//> n
        filter' pth (PlusTestOptions o t) = PlusTestOptions o <$> filter' pth t
        -- we don't know at tree construction time what the tree wrapped inside an AskOptions/WithResource
        -- is going to look like. We always return something, and just return an empty test group
        -- if later on we see that the child subtree was excluded.
        filter' pth (WithResource r t) = Just $ WithResource r (\x -> fromMaybe emptyTest (filter' pth (t x)))
        filter' pth (AskOptions t) = Just $ AskOptions (\o -> fromMaybe emptyTest (filter' pth (t o)))

        emptyTest = testGroup "" []

