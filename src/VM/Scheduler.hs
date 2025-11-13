{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.Scheduler where

import Data.Word
import Data.Int
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.List as L
import Data.Ord
import Data.Time.Clock

-- Task priority levels
data Priority = Low | Normal | High | Critical
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

priorityValue :: Priority -> Int
priorityValue Low = 1
priorityValue Normal = 2
priorityValue High = 3
priorityValue Critical = 4

-- Task for scheduling
data Task = Task
  { taskId :: Word32
  , taskPriority :: Priority
  , taskFunction :: IO ()
  , taskCreated :: UTCTime
  }

-- Task scheduler
data TaskScheduler = TaskScheduler
  { schedulerQueue :: TVar [Task]
  , schedulerRunning :: TVar Bool
  , schedulerThread :: Maybe ThreadId
  , schedulerMaxConcurrent :: Int
  , schedulerActiveTasks :: TVar Int
  }

-- Create task scheduler
createScheduler :: Int -> IO TaskScheduler
createScheduler maxConcurrent = do
  queue <- newTVarIO []
  running <- newTVarIO False
  active <- newTVarIO 0
  return $ TaskScheduler
    { schedulerQueue = queue
    , schedulerRunning = running
    , schedulerThread = Nothing
    , schedulerMaxConcurrent = maxConcurrent
    , schedulerActiveTasks = active
    }

-- Schedule a task
scheduleTask :: TaskScheduler -> Priority -> IO () -> IO Word32
scheduleTask scheduler priority taskFunc = do
  taskId <- generateTaskId
  now <- getCurrentTime
  let task = Task
        { taskId = taskId
        , taskPriority = priority
        , taskFunction = taskFunc
        , taskCreated = now
        }
  atomically $ do
    queue <- readTVar (schedulerQueue scheduler)
    -- Insert task maintaining priority order (highest first)
    let priorityVal = priorityValue priority
        newQueue = L.insertBy (\t1 t2 -> compare (priorityValue $ taskPriority t2) (priorityValue $ taskPriority t1)) task queue
    writeTVar (schedulerQueue scheduler) newQueue
  return taskId

-- Generate unique task ID
generateTaskId :: IO Word32
generateTaskId = do
  tid <- myThreadId
  return $ fromIntegral $ hash (show tid)

hash :: String -> Int
hash = foldl (\acc c -> acc * 31 + fromEnum c) 0

-- Start scheduler
startScheduler :: TaskScheduler -> IO ()
startScheduler scheduler = do
  atomically $ writeTVar (schedulerRunning scheduler) True
  tid <- forkIO $ schedulerLoop scheduler
  return ()

-- Scheduler loop
schedulerLoop :: TaskScheduler -> IO ()
schedulerLoop scheduler = do
  running <- readTVarIO (schedulerRunning scheduler)
  if not running
    then return ()
    else do
      active <- readTVarIO (schedulerActiveTasks scheduler)
      if active < schedulerMaxConcurrent scheduler
        then do
          task <- atomically $ do
            queue <- readTVar (schedulerQueue scheduler)
            case queue of
              (t:ts) -> do
                writeTVar (schedulerQueue scheduler) ts
                active' <- readTVar (schedulerActiveTasks scheduler)
                writeTVar (schedulerActiveTasks scheduler) (active' + 1)
                return $ Just t
              [] -> return Nothing
          case task of
            Just t -> do
              forkIO $ do
                taskFunction t
                atomically $ do
                  active' <- readTVar (schedulerActiveTasks scheduler)
                  writeTVar (schedulerActiveTasks scheduler) (active' - 1)
            Nothing -> threadDelay 10000  -- 10ms delay if no tasks
        else threadDelay 1000  -- 1ms delay if at max capacity
      schedulerLoop scheduler

-- Stop scheduler
stopScheduler :: TaskScheduler -> IO ()
stopScheduler scheduler = do
  atomically $ writeTVar (schedulerRunning scheduler) False

