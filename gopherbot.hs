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
import Control.Monad(when)
import System.Directory
import DB
import Database.HSQL
import Utils
import MissingH.Path.FilePath
import MissingH.Network
import NetClient
import DirParser
import Control.Concurrent

main = niceSocketsDo $
    do setCurrentDirectory baseDir
       l <- newLock
       c <- initdb
       runScan l c
       disconnect c

runScan l c =
    do n <- numToProc c
       putStrLn $ (show n) ++ " items to process"
       if n == 0
          then do mapM_ (\g -> updateItem l c g NotVisited) startingAddresses
          else return ()
       procLoop l c

procLoop lock c =
    do n <- numToProc c
       i <- popItem lock c
       case i of
         Nothing -> do putStrLn $ "Exiting with queue size " ++ (show n)
                       return ()
         Just item -> do procItem lock c n item
                         procLoop lock c

procItem lock c n item =
    do t <- myThreadId
       putStrLn $ (show t) ++ " #" ++ (show n) ++ ": " ++ (show item)
       let fspath = getFSPath item
       acquire lock             -- We don't want to stomp on each others mkdir
       createDirectoryIfMissing True (fst . splitFileName $ fspath)
       release lock
       catch (do dlItem item fspath
                 when (dtype item == '1') (spider lock c fspath)
                 updateItem lock c item Visited
             )
          (\e -> do putStrLn $ "Error: " ++ (show e)
                    updateItem lock c item ErrorState)

spider l c fspath =
    do netreferences <- parseGMap fspath
       let refs = filter filt netreferences
       queueItems l c refs
    where filt a = (dtype a) /= 'i' &&
                   not (host a `elem` excludeServers)