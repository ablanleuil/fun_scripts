{-# LANGUAGE Strict, ScopedTypeVariables #-}

import Data.Time.Clock
import Data.Time.Calendar
import Data.Char
import Data.List.Split (splitOn)
import Text.Shaun
import Text.Shaun.Sweeper
import Text.Shaun.Types (insert, append)
import System.IO hiding (hGetContents)
import System.IO.Strict
import System.Environment ( getArgs, getExecutablePath )
import System.FilePath.Posix ( (</>), dropFileName )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catchAll)
import Control.Monad
import Control.Exception (bracket)
import Data.Maybe
import Data.List (sort, reverse)
import Data.Function (on)

-- Integer intervals (constant represented by [X; X], infinity by To/From)
data Slice = From Int | To Int | Between Int Int

-- A task's item
data Item = Item { desc :: String, date :: String, check :: Bool, prio :: Int } deriving (Eq)

instance Ord Item where
  (<=) a b = check a && not (check b) || (check a == check b && prio a <= prio b)

instance Show Item where
  show (Item de da c p) = "[" ++ (if c then "X" else " ") ++ "] " ++ de ++ " " ++ da ++ " [" ++ show p ++ "]"

instance Shaun Item where
  toShaun (Item de da c p) = SObject [ ("desc", toShaun de)
                                     , ("date", toShaun da)
                                     , ("check", toShaun c)
                                     , ("prio", toShaun p) ]
  fromShaun (SObject m) = Item (get "desc") (get "date") (get "check") (get "prio")
    where get s = fromShaun $ fromJust $ lookup s m
  fromShaun _ = error "Cannot compute Item: bad ShaunValue"

instance Show Slice where
  show (From a) = show a ++ "-"
  show (To a) = "-" ++ show a
  show (Between a b) = show a ++ "-" ++ show b

instance Read Slice where
  readsPrec _ ('-':num) = [(To $ read num, "")]
  readsPrec _ (input@(x:xs)) =
    case last input of
      '-' -> [(From $ read $ init input, "")]
      a | isDigit a -> if isDigit x then
                         case map read (splitOn "-" input) of
                          [a]   -> [(Between a a, "")]
                          [a,b] -> [(Between a b, "")]
                       else []

-- Gets the date in yyyy-mm-dd format
getDate :: IO String
getDate = fmap ((\(y,m,d) -> show y ++ "-" ++ show m ++ "-" ++ show d)
                           . toGregorian
                           . utctDay)
               getCurrentTime

-- Creates an item given a description and a priority, with the current date
mkIt :: String -> Int -> IO Item
mkIt s p = do
  d <- getDate
  return $ Item s d False p

-- Checks if an integer is captured by a slice
into :: Int -> Slice -> Bool
into x (From a) = x >= a
into x (To a)   = x <= a
into x (Between a b) = x >= a && x <= b

-- Prints all tasks name
printList :: SweeperT IO ()
printList = getTo "tasks" >>= liftIO
                              . sequence_
                              . map (putStrLn . fst)
                              . (\(SObject ts) -> ts)

-- Sets all tasks in the given slices to 'checked'
checkTask :: String -> [Slice] -> SweeperT IO ()
checkTask name slices = do
  to "tasks"
  (SList items) <- getTo name

  set $ toShaun $ zipWith
    (\id it -> it { check = check it || any (into id) slices })
    [1..]
    $ reverse $ sort $ map fromShaun items -- Sort in descending priority order

-- Removes all items in a task+the task itself
removeTasks :: [String] -> SweeperT IO ()
removeTasks names = do
  to "tasks"
  modify (\(SObject ts) -> SObject [(x,y) | (x,y) <- ts, not (elem x names)])

-- Adds an item to a task, creates it if inexistent
addTask :: String -> String -> [String] -> SweeperT IO ()
addTask name content (prio:_) = do
  to "tasks"

  catchAll
    -- If task exists, [to name] won't fail
    (to name >> do
      i <- liftIO $ mkIt content (read prio)
      modify $ append (toShaun i))

    -- If [to name] fails, adds a singleton task
    (\_ -> do
      i <- liftIO $ mkIt content (read prio)
      modify $ insert (name, toShaun [i]))

addTask name content [] = addTask name content ["0"]

-- Shows a task's items in descending order (unchecked first, then by priority)
showTask :: String -> SweeperT IO [String]
showTask t = do
  (SObject o) <- getTo "tasks"
  return $ case lookup t o of
    Just (SList l) -> zipWith (\i (s :: Item) -> show i ++ ": " ++ show s)
                              [1..]
                              $ reverse $ sort $ map fromShaun l
    _ -> []

showAllTasks :: SweeperT IO ()
showAllTasks = do
  (SObject o) <- peek $ getTo "tasks"
  sequence_ (map (\(s,v) -> do
    items <- showTask s
    liftIO $ do
      putStrLn (s++":")
      putStrLn $ unlines $ map ("  "++) items
    top) o)

printHelp :: SweeperT IO ()
printHelp = do
  doc <- getTo "doc"
  case doc of
    SString s -> liftIO $ putStrLn s
    _ -> liftIO $ print doc

dumpTask :: String -> SweeperT IO String
dumpTask t = do
  obj <- peek $ path ("tasks:"++t) >> get
  return $ unlines
           . map (show :: Item -> String) -- Ensures fromShaun to return [Item]
           . fromShaun
           $ obj

dumpTasks :: [String] -> SweeperT IO ()
dumpTasks l = do
  let (names, file) = (init l, last l)

  -- Gathers all tasks representation
  ret <- fmap unlines
            $ sequence
            $ map dumpTask names

  -- Writes the file
  liftIO $ bracket (openFile file WriteMode) hClose (\h -> hPutStr h ret)

main = do
  args <- getArgs
  dir <- fmap dropFileName getExecutablePath

  let filename = dir </> "todo.sn"

  str <- bracket (openFile filename ReadMode) hClose hGetContents

  let content = read str

  let action = case args of {
    ("list":_) -> printList;
    ("remove":names) -> removeTasks names;
    ("check":name:numbers) -> checkTask name (map read numbers);
    ("help":_) -> printHelp;
    ("add":name:content:rest) -> addTask name content rest;
    ("showall":_) -> showAllTasks;
    ("show":name:_) -> showTask name >>= liftIO . putStrLn . unlines;
    ("dump":rest) -> dumpTasks rest;
    _ -> printHelp }

  ret <- withSweeperT (peek action >> get) content
   
  -- Re-writes the file if any modification was done
  if ret /= content then
    bracket (openFile filename WriteMode) hClose (\h -> hPutStr h $ show ret)
  else return ()

