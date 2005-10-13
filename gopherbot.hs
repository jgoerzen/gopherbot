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
       children <- mapM (\_ -> myForkIO (procLoop l gasupply c)) [1..numThreads]
       stats <- forkIO (statsthread l c)
       supplier <- forkIO (nextFinder gasupply c)
       waitForChildren children
       

myForkIO :: IO () -> IO (MVar ())
myForkIO io =
    do mvar <- newEmptyMVar
       forkIO (action `finally` putMVar mvar ())
       return mvar
    where action = do t <- myThreadId
                      msg "started."
                      io

waitForChildren :: [MVar ()] -> IO ()
waitForChildren [] = return ()
waitForChildren (c:xs) =
    do takeMVar c
       waitForChildren xs

procLoop lock gasupply c =
    do i <- popItem lock gasupply c
       procLoop' lock gasupply c i

procLoop' lock gasupply c i =
    do case i of
         Nothing -> msg $ "Exiting"
         Just item -> do procItem lock c item
                         -- Popping the next item before releasing the current
                         -- host is a simple form of being nice to remotes
                         i <- popItem lock gasupply c
                         procLoop' lock gasupply c i
data RobotStatus = RobotsOK     -- ^ Proceed
                 | RobotsDeny   -- ^ Stop
                 | RobotsError  -- ^ Error occured; abort.

    
checkRobots :: Lock -> Connection -> GAddress -> IO RobotStatus
checkRobots lock c ga =
    do let fspath = getFSPath garobots
       dfe <- doesFileExist fspath
       unless (dfe) (procItem lock c garobots)
       dfe2 <- doesFileExist fspath
       if dfe2
          then do r <- parseRobots fspath
                  return $ case isURLAllowed r "gopherbot" (path ga) of
                                True -> RobotsOK
                                False -> RobotsDeny
          else return RobotsError
          
    where garobots = ga {path = "robots.txt", dtype = '0'}

-- TODO: better crash handling on robots.txt

procItem lock c item = procIfRobotsOK lock c item $
    do t <- myThreadId
       msg $ show item
       let fspath = getFSPath item
       catch (bracket_ (acquire lock) (release lock) (createDirectoryIfMissing True (fst . splitFileName $ fspath)))
             (\e -> do msg $ "Single-Item Error on " ++ (show item) ++ ": " 
                           ++ (show e)
                       updateItem lock c item ErrorState
             )
       catch (do dlItem item fspath
                 when (dtype item == '1') (spider lock c fspath)
                 updateItem lock c item Visited
             )
          (\e -> do msg $ "Error on " ++ (show item) ++ ": " ++ (show e)
                    noteErrorOnHost lock c (host item)
          )
                  

procIfRobotsOK :: Lock -> Connection -> GAddress -> IO () -> IO ()
procIfRobotsOK lock c item action =
              do r <- if (path item /= "robots.txt")
                          then checkRobots lock c item
                          else return RobotsOK -- Don't try to re-process robots.txt itself
                 case r of
                    RobotsOK -> action
                    RobotsDeny -> do msg $ "Excluded by robots.txt: " ++ (show item)
                                     updateItem lock c item ErrorState
                    RobotsError -> do msg $ "Blocking host due to connection problems with robots.txt: " ++ host item
                                      noteErrorOnHost lock c (host item)


spider l c fspath =
    do netreferences <- parseGMap fspath
       let refs = filter filt netreferences
       queueItems l c refs
    where filt a = (not ((dtype a) `elem` ['i', '3', '8', '7', '2'])) &&
                   not (host a `elem` excludeServers)

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
                 
