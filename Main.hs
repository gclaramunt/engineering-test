module Main (main) where

import Data.List
import Distribution.TestSuite (Result)

-- * An executor should have a maximum amount of tasks it can execute concurrently.

-- For this assignment's purposes, you may employ task types like "Bubble Bath Optimization," "Squirrel Patrol," and "Unicorn Wrangling," each executed

-- * Each task carries a unique identifier, the task type, and input parameters, depending on the type of task.

-- * -------------------------------------------

-- * An executor is a server process that continously exchanges messages with the driver.

-- * Executor and driver communicate over STDIN and STDOUT channels for simplicity.

-- * Upon task completion, the executor will promptly notify the driver of the task result, which may include success or failure status, along with any task-specific result value generated.

-- * For long-running tasks, the executor will periodically send status updates to the driver, ensuring transparency in task progress.

-- * The driver reserves the right to request task cancellation mid-execution, adding flexibility to task management.

-- * Cancellation or failure of one task shoudn't influence other tasks running concurrently.

-- with a simulated delay using a sleep function. Duration of the sleep could be denoted as per-task input parameter.

type TaskId = Int

type Time = Int

data TaskDefinition = TaskDefinition {task_id :: TaskId, task_definition :: Task} deriving (Show)

data Task = BubbleBathOptimization | SquirrelPatrol String | UnicornWrangling String String deriving (Show)

-- predefined duration for each task
duration BubbleBathOptimization = 100
duration (SquirrelPatrol _) = 200
duration (UnicornWrangling _ _) = 300

data TaskResult = Success String | Failure String deriving (Show)

data TaskStatus = Running | Result TaskResult deriving (Show)

data TaskRunInfo = TaskRunInfo {task :: TaskDefinition, start_time :: Int, status :: TaskStatus} deriving (Show)

data Executor = Executor {max_tasks :: Int, tasks :: [TaskRunInfo]}

data Simulation = Simulation {executor :: Executor, time :: Time}

data ExecutorOp = Submit TaskDefinition | Cancel TaskId | Status | Advance Time

isDone current_time task_run_info = start_time task_run_info + duration task' < current_time
  where
    task' = task_definition $ task task_run_info

-- protocol actions
parse :: [String] -> ExecutorOp
parse ("submit" : id : taskType : params) = Submit $ TaskDefinition (read id) task
  where
    task = case taskType of
      "BubbleBathOptimization" -> BubbleBathOptimization
      "SquirrelPatrol" -> SquirrelPatrol p1 where p1 = head params
      "UnicornWrangling" -> UnicornWrangling p1 p2 where [p1, p2] = params
      _ -> error "Unknown task"
parse ["cancel", id] = Cancel $ read id
parse ["status"] = Status
parse ["(advance)", time] = Advance $ read time
parse _ = error "Unknown command"

taskResult task_ri =
  if task_id (task task_ri) `mod` 10 == 3
    then Success $ "finished task" ++ show task_ri
    else Failure $ "task failed" ++ show task_ri

-- submit a task
simulationStep :: Simulation -> ExecutorOp -> (Simulation, [TaskResult])
simulationStep sim@(Simulation Executor {max_tasks = max_tasks, tasks = tasks} time) (Submit task_def) =
  if length tasks < max_tasks
    then (Simulation (Executor max_tasks (TaskRunInfo task_def time Running : tasks)) time, [])
    else (sim, [Failure "Exceded max number of running tasks"])
-- cancel task
simulationStep (Simulation Executor {max_tasks = max_tasks, tasks = tasks} time) (Cancel task_id') =
  (Simulation (Executor max_tasks still_running) time, [Failure ("Cancelled task " ++ show task_id')])
  where
    still_running = filter (\t_id -> task_id' == task_id (task t_id)) tasks
-- advance simulation time, finishing the tasks completed in that period
simulationStep (Simulation Executor {max_tasks = max_tasks, tasks = tasks} current_time) (Advance time) =
  (Simulation (Executor max_tasks still_running) new_time, results)
  where
    new_time = current_time + time
    (finished, still_running) = partition (isDone new_time) tasks
    results = map taskResult finished
simulationStep sim@(Simulation Executor {max_tasks = max_tasks, tasks = tasks} current_time) Status =
  (sim, results)
  where
    results = map taskResult tasks

simulationLoop sim = do
  input <- readLn
  let op = parse input
  let (new_sim, results) = simulationStep sim op
  print results
  simulationLoop new_sim

main :: IO ()
main = simulationLoop $ Simulation Executor {max_tasks = 10, tasks = []} 0