{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main where

import Config
import Control.Monad(when, unless)
import Control.Exception(finally)
import System.Directory
import DB
import Database.HSQL
import Utils
import MissingH.Path.FilePath
import MissingH.Network
import NetClient
import DirParser
import Control.Concurrent
import Data.List
import Control.Exception(bracket_)
import RobotsTxt

{- | Main entry point for the program. -}
main = niceSocketsDo $          -- Prepare things for sockets
    do setCurrentDirectory baseDir -- chdir to the working dir
       l <- newLock             -- Global lock for db updates
       c <- initdb              -- Initialize the database and get a conn
       gasupply <- newEmptyMVar -- Global MVar for the supply of selectors
       runScan gasupply l c     -- main scanner
       disconnect c             -- shut down

{- | Set up all the threads and get them going. -}
runScan gasupply l c =
    do n <- numToProc c
       msg $ (show n) ++ " items to process"
       when (n == 0)            -- Nothing to do: prime the db
          (mapM_ (\g -> updateItem l c g NotVisited) startingAddresses)
       {- Fork off the childthreads.  Each one goes into a loop
          of waiting for new items to process and processing them. -}
       children <- mapM 
                   (\_ -> myForkIO (procLoop l gasupply c)) [1..numThreads]
       -- This is the thread that displays status updates every so often
       stats <- forkIO (statsthread l c)
       -- And this is the thread that supplies items to process
       supplier <- forkIO (nextFinder gasupply c)
       -- When the main thread exits, so does the program, so
       -- we wait for all children before exiting.
       waitForChildren children
       
{- | A simple wrapper around forkIO to notify the main thread when each
individual thread dies. -}
myForkIO :: IO () -> IO (MVar ThreadId)
myForkIO io =
    do mvar <- newEmptyMVar
       t <- myThreadId
       forkIO (action `finally` putMVar mvar t)
       return mvar
    where action = do msg "started."
                      io

{- | Wait for child threads to die. 

This should only happen when there is nothing else to spider. -}
waitForChildren :: [MVar ThreadId] -> IO ()
waitForChildren [] = msg $ "All children died; exiting."
waitForChildren (c:xs) =
    do t <- takeMVar c
       msg $ " *********** Thread died: " ++ (show t)
       waitForChildren xs

{- | Main entry point for each worker thread.  We just pop the first item,
then call procLoop'. -}
procLoop lock gasupply c =
    do i <- popItem lock gasupply c
       procLoop' lock gasupply c i

{- | Main worker loop.  We receive an item and process it.  If it's
Nothing, there is nothing else to do, so the thread shuts down. 
Otherwise, call procItem, pop the next, and then call itself. -}
procLoop' lock gasupply c i =
    do case i of
         Nothing -> msg $ "Exiting"
         Just item -> do procItem lock c item
                         -- Popping the next item before releasing the current
                         -- host is a simple form of being nice to remotes
                         i <- popItem lock gasupply c
                         procLoop' lock gasupply c i

{- | What happened when we checked the robots.txt file? -}
data RobotStatus = RobotsOK     -- ^ Proceed
                 | RobotsDeny   -- ^ Do not download this file
                 | RobotsError  -- ^ Error occured; abort.

{- | Given a 'GAddress' (corresponding to a single item),
check to see if it's OK to download according to robots.txt.
-}
checkRobots :: Lock -> Connection -> GAddress -> IO RobotStatus
checkRobots lock c ga =
    do let fspath = getFSPath garobots
       dfe <- doesFileExist fspath
       unless (dfe) (procItem lock c garobots) -- Download file if needed
       dfe2 <- doesFileExist fspath -- Do we have it yet?
       if dfe2
          then -- Yes.  Parse it, and see what happened.
               do r <- parseRobots fspath
                  return $ case isURLAllowed r "gopherbot" (path ga) of
                                True -> RobotsOK
                                False -> RobotsDeny
          else return RobotsError -- No.  TCP error occured.
          
    where garobots = ga {path = "robots.txt", dtype = '0'}

{- | Run an IO action, but only if it's OK according to robots.txt. -}
procIfRobotsOK :: Lock -> Connection -> GAddress -> IO () -> IO ()
procIfRobotsOK lock c item action =
              do r <- if (path item /= "robots.txt")
                          then checkRobots lock c item
                          else -- Don't try to check if robots.txt itself is ok
                               return RobotsOK
                 case r of
                    RobotsOK -> action -- OK per robots.txt; run it.
                    RobotsDeny -> do msg $ "Excluded by robots.txt: " ++ 
                                             (show item)
                                     updateItem lock c item ErrorState
                    RobotsError -> do msg $ "Problem getting robots.txt: " ++ 
                                          host item
                                      noteErrorOnHost lock c (host item)

-- TODO: better crash handling on robots.txt

{- | OK, we have an item.  If it's OK according to robots.txt, download
and process it. -}
procItem lock c item = procIfRobotsOK lock c item $
    do msg $ show item          -- Show what we're up to
       let fspath = getFSPath item

       -- Create the directory for the file to go in, if necessary.
       catch (bracket_ (acquire lock) 
                       (release lock) 
                       (createDirectoryIfMissing True 
                        (fst . splitFileName $ fspath)))
             (\e -> -- If we got an exception here, note an error for this item
                    do msg $ "Single-Item Error on " ++ (show item) ++ ": " 
                           ++ (show e)
                       updateItem lock c item ErrorState
             )

       -- Now, download it.  If it's a menu (item type 1), check it for links
       -- (spider it).  Error here means a TCP problem, so mark every
       -- item on this host as having the error.
       catch (do dlItem item fspath
                 when (dtype item == '1') (spider lock c fspath)
                 updateItem lock c item Visited
             )
          (\e -> do msg $ "Error on " ++ (show item) ++ ": " ++ (show e)
                    noteErrorOnHost lock c (host item)
          )
                  
{- | This function is called by procItem whenever it downloads a
menu.  This function calles the parser, extracts items, and calles
DB.queueItems to handle them.  (Insert into DB if new) -}
spider l c fspath =
    do netreferences <- parseGMap fspath
       let refs = filter filt netreferences
       queueItems l c refs
    where filt a = (not ((dtype a) `elem` ['i', '3', '8', '7', '2'])) &&
                   not (host a `elem` excludeServers)

{- | This thread prints a periodic status update. -}
statsthread :: Lock -> Connection -> IO ()
statsthread l c =
    do total <- getCount c "1 = 1"
       let totaltext = show total ++ " total"
       statetxts <- mapM (procstate total) states
       let disp = concat . intersperse ", " $ totaltext : statetxts
       msg disp
       threadDelay (60 * 1000000)
       statsthread l c
    where states = [NotVisited, VisitingNow, Visited, ErrorState]
          procstate total s =
              do r <- getCount c ("state = " ++ toSqlValue (show s))
                 let pct = r * 100 `div` total
                 return $ show r ++ " (" ++ show pct ++ "%) " ++ (show s)
                 
