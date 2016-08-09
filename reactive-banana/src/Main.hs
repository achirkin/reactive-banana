module Main where

import Control.Monad (forM, forM_, forever)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Control.Concurrent (threadDelay)
import qualified Reactive.Banana.Frameworks as RBFrameworks
import qualified Reactive.Banana.Types as RBTypes
import qualified Reactive.Banana.Internal.Combinators as Prim

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Hashable
import           Data.IORef
import           Data.Maybe                    (catMaybes, mapMaybe)
--import           Data.Unique.Really
import qualified GHC.Base               as GHC
import qualified GHC.IORef              as GHC
import qualified GHC.STRef              as GHC
import qualified GHC.Weak               as GHC
import           System.Mem.Weak
import           System.Mem
import           System.Exit


hNUM :: Int
hNUM = 15

data Val = A Int | B Int

main :: IO ()
main = do
    (reg, run) <- genEvents
    performGC
    handlers <- makeHandlers hNUM reg
    performGC
    network <- RBFrameworks.compile . forM_ handlers $ \ah -> do
          liftIO performGC
          ev <- RBFrameworks.fromAddHandler ah
          liftIO performGC
          RBFrameworks.reactimate $ ourAction <$> ev
    RBFrameworks.actuate network
    performGC
    forM_ [1..5] $ const run
    RBFrameworks.pause network
    exitSuccess
  where
    ourAction x = do
      performGC
      threadDelay 10000
      putStrLn $ "--------------  DOING ACTION  --------- " ++ show x

genEvents :: IO ((Int -> IO ()) -> IO (), IO ())
genEvents = do
  handlers <- newIORef []
  return ( \h -> modifyIORef handlers (h:)
         , readIORef handlers >>= mapM_ ($ 0)
         )

makeHandlers :: Int -> ((Int -> IO ()) -> IO ()) -> IO [RBFrameworks.AddHandler Int]
makeHandlers n reg = forM [1..n] $ \i -> do
    (ah, fire) <- RBFrameworks.newAddHandler
    reg $ fire . (+i)
    return ah
