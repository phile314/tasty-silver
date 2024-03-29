{-# LANGUAGE RankNTypes, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Test.Tasty.Silver.Advanced
  ( -- * Constructing golden tests
    goldenTest1,

    goldenTestIO,
    goldenTestIO1,
    goldenTest,

    GShow (..),
    GDiff (..),

    -- * Reading files
    readFileMaybe
  )
where

#if !(MIN_VERSION_base(4,8,0))
import Data.Functor ( (<$>) )
#endif

import Test.Tasty.Providers
import Test.Tasty.Silver.Internal
import qualified Data.Text as T

-- | A very general testing function. Use 'goldenTest1' instead if you can.
goldenTest
  :: TestName -- ^ test name
  -> (IO a) -- ^ get the golden correct value
  -> (IO a) -- ^ get the tested value
  -> (a -> a -> IO (Maybe String))
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> IO ()) -- ^ update the golden file
  -> TestTree
goldenTest t golden test cmp upd = goldenTestIO t (Just <$> golden) test runCmp shw upd
  where  -- the diff should behave in a pure way, so let's just use unsafePerformIO
        runCmp a b = do
            cmp' <- cmp a b
            case cmp' of
                Just d -> return $ ShowDiffed Nothing (T.pack d)
                Nothing -> return Equal
        shw _ = return $ ShowText "Old API does not support showing the actual value. Use the --accept mode or use the new API."


-- | A very general testing function.
goldenTest1
  :: TestName -- ^ test name
  -> (IO (Maybe a)) -- ^ get the golden correct value
  -> (IO a) -- ^ get the tested value
  -> (a -> a -> GDiff)
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Equal'. If they are
    -- different, it should return a diff representation.
    -- First argument is golden value.
  -> (a -> GShow) -- ^ Show the golden/actual value.
  -> (a -> IO ()) -- ^ update the golden file
  -> TestTree
goldenTest1 t golden test diff shw upd = goldenTestIO t golden test (\a b -> return $ diff a b) (return . shw) upd

-- | A very general testing function.
-- The IO version of show/diff are useful when using external diff or show mechanisms. If IO is not required,
-- the `goldenTest1` function should be used instead.
goldenTestIO
  :: TestName -- ^ test name
  -> (IO (Maybe a)) -- ^ get the golden correct value
  -> (IO a) -- ^ get the tested value
  -> (a -> a -> IO GDiff)
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Equal'. If they are
    -- different, it should return a diff representation.
    -- First argument is golden value.
  -> (a -> IO GShow) -- ^ Show the golden/actual value.
  -> (a -> IO ()) -- ^ update the golden file
  -> TestTree
goldenTestIO t golden test diff shw upd = goldenTestIO1 t golden test diff shw (Just upd)

-- | A very general testing function.
-- Experimental function, may change in later versions!
-- The IO version of show/diff are useful when using external diff or show mechanisms. If IO is not required,
-- the `goldenTest1` function should be used instead.
goldenTestIO1
  :: TestName -- ^ test name
  -> (IO (Maybe a)) -- ^ get the golden correct value
  -> (IO a) -- ^ get the tested value
  -> (a -> a -> IO GDiff)
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Equal'. If they are
    -- different, it should return a diff representation.
    -- First argument is golden value.
  -> (a -> IO GShow) -- ^ Show the golden/actual value.
  -> (Maybe (a -> IO ())) -- ^ update the golden file
  -> TestTree
goldenTestIO1 t golden test diff shw upd = singleTest t $ Golden golden test diff shw upd
