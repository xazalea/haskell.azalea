{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.Performance where

import Data.Word
import Data.Int
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock
import qualified Data.Map.Strict as M

-- Performance metrics
data PerformanceMetrics = PerformanceMetrics
  { metricsInstructionsPerSecond :: TVar Int
  , metricsMemoryUsage :: TVar Word64
  , metricsCPUUsage :: TVar Double
  , metricsFPS :: TVar Int
  , metricsFrameTime :: TVar Double
  , metricsCacheHits :: TVar Int
  , metricsCacheMisses :: TVar Int
  , metricsStartTime :: UTCTime
  }

-- Create performance metrics tracker
createPerformanceMetrics :: IO PerformanceMetrics
createPerformanceMetrics = do
  startTime <- getCurrentTime
  ips <- newTVarIO 0
  mem <- newTVarIO 0
  cpu <- newTVarIO 0.0
  fps <- newTVarIO 0
  frameTime <- newTVarIO 0.0
  hits <- newTVarIO 0
  misses <- newTVarIO 0
  return $ PerformanceMetrics
    { metricsInstructionsPerSecond = ips
    , metricsMemoryUsage = mem
    , metricsCPUUsage = cpu
    , metricsFPS = fps
    , metricsFrameTime = frameTime
    , metricsCacheHits = hits
    , metricsCacheMisses = misses
    , metricsStartTime = startTime
    }

-- Update IPS
updateIPS :: PerformanceMetrics -> Int -> IO ()
updateIPS metrics ips = atomically $ writeTVar (metricsInstructionsPerSecond metrics) ips

-- Update memory usage
updateMemoryUsage :: PerformanceMetrics -> Word64 -> IO ()
updateMemoryUsage metrics mem = atomically $ writeTVar (metricsMemoryUsage metrics) mem

-- Update CPU usage
updateCPUUsage :: PerformanceMetrics -> Double -> IO ()
updateCPUUsage metrics cpu = atomically $ writeTVar (metricsCPUUsage metrics) cpu

-- Update FPS
updateFPS :: PerformanceMetrics -> Int -> IO ()
updateFPS metrics fps = atomically $ writeTVar (metricsFPS metrics) fps

-- Update frame time
updateFrameTime :: PerformanceMetrics -> Double -> IO ()
updateFrameTime metrics time = atomically $ writeTVar (metricsFrameTime metrics) time

-- Record cache hit
recordCacheHit :: PerformanceMetrics -> IO ()
recordCacheHit metrics = atomically $ do
  hits <- readTVar (metricsCacheHits metrics)
  writeTVar (metricsCacheHits metrics) (hits + 1)

-- Record cache miss
recordCacheMiss :: PerformanceMetrics -> IO ()
recordCacheMiss metrics = atomically $ do
  misses <- readTVar (metricsCacheMisses metrics)
  writeTVar (metricsCacheMisses metrics) (misses + 1)

-- Get cache hit rate
getCacheHitRate :: PerformanceMetrics -> IO Double
getCacheHitRate metrics = atomically $ do
  hits <- readTVar (metricsCacheHits metrics)
  misses <- readTVar (metricsCacheMisses metrics)
  let total = hits + misses
  if total == 0
    then return 0.0
    else return $ fromIntegral hits / fromIntegral total

-- Get performance metrics as JSON
getPerformanceMetricsJSON :: PerformanceMetrics -> IO Value
getPerformanceMetricsJSON metrics = do
  ips <- readTVarIO (metricsInstructionsPerSecond metrics)
  mem <- readTVarIO (metricsMemoryUsage metrics)
  cpu <- readTVarIO (metricsCPUUsage metrics)
  fps <- readTVarIO (metricsFPS metrics)
  frameTime <- readTVarIO (metricsFrameTime metrics)
  hitRate <- getCacheHitRate metrics
  return $ object
    [ "ips" .= ips
    , "memory" .= mem
    , "cpu" .= cpu
    , "fps" .= fps
    , "frameTime" .= frameTime
    , "cacheHitRate" .= hitRate
    ]

