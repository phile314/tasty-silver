import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Test.Tasty.Silver
import Test.Tasty.Silver.Interactive
import Test.Tasty.Runners
import Test.Tasty.Silver.Advanced
import Test.Tasty.Silver.Internal
import Test.Tasty.Options
import Test.Tasty.Silver.Filter

import Data.Monoid
import System.IO.Temp
import System.FilePath
import System.Directory
import Data.List (sort)
import Control.Concurrent.MVar
import Control.Monad.IO.Class

touch f = writeFile f ""

main = defaultMain $ testGroup "tests" [testFindByExt, testWithResource, testCheckRF]



testFindByExt :: TestTree
testFindByExt = testCase "findByExtension" $
    withSystemTempDirectory "golden-test" $ \basedir -> do

      setCurrentDirectory basedir

      createDirectory ("d1")
      createDirectory ("d1" </> "d2")
      touch ("f1.c")
      touch ("f2.h")
      touch ("f2.exe")
      touch ("d1" </> "g1.c")
      touch ("d1" </> "d2" </> "h1.c")
      touch ("d1" </> "d2" </> "h1.exe")
      touch ("d1" </> "d2" </> "h1")

      files <- findByExtension [".c", ".h"] "."
      sort files @?= sort
        ["./d1/d2/h1.c","./d1/g1.c","./f1.c","./f2.h"]

testWithResource :: TestTree
testWithResource = testCase "withResource" $ do
    -- check if resources are properly initialized
    testRes <- newMVar False
    let acq = newMVar True
        free v = swapMVar v False >> return ()
        test = \v -> goldenTest1 "check res" (return Nothing) (liftIO $ testAction v) (\_ _ -> Equal) (\_ -> ShowText mempty) upd
        upd x = assertBool "Incorrect result" x >> return ()
        testAction = \v -> v >>= readMVar >>= assertBool "Resource not initialized." >> return True
        tree = withResource acq free test

    let r = tryIngredients [consoleTestReporter] (singleOption $ AcceptTests True) tree
    case r of
      Just r' -> do
        success <- r'
        assertBool "Test should succeed." success
      Nothing -> assertFailure "Test broken"

testCheckRF :: TestTree
testCheckRF = testGroup "Filter.checkRF"
    [ testCase "empty1" $ checkRF True [] "/" @?= True
    , testCase "empty1" $ checkRF False [] "/" @?= False
    , testCase "empty2" $ checkRF True [] "/sdfg" @?= True
    , testCase "empty2" $ checkRF False [] "/sdfg" @?= False
    ]
