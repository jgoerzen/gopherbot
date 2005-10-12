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

main = niceSocketsDo $
    do setCurrentDirectory baseDir
       c <- initdb
       runScan c
       disconnect c

runScan c =
    do n <- numToProc c
       putStrLn $ (show n) ++ " items to process"
       if n == 0
          then do mapM_ (\g -> updateItem c g NotVisited) startingAddresses
          else return ()
       n' <- numToProc c
       procLoop c

procLoop c =
    do n <- numToProc c
       i <- popItem c
       case i of
         Nothing -> do putStrLn $ "Exiting with queue size " ++ (show n)
                       return ()
         Just item -> do procItem c n item
                         procLoop c

procItem c n item =
    do putStrLn $ "Processing #" ++ (show n) ++ ": " ++ (show item)
       let fspath = getFSPath item
       createDirectoryIfMissing True (fst . splitFileName $ fspath)
       catch (do dlItem item fspath
                 when (dtype item == '1') (spider c item fspath)
                 updateItem c item Visited
             )
          (\e -> do putStrLn $ "Error: " ++ (show e)
                    updateItem c item ErrorState)

spider c item fspath =
    do netreferences <- parseGMap fspath
       let refs = filter filt netreferences
       mapM_ (\a -> queueItem c a) refs
    where filt a = (dtype a) /= 'i' &&
                   not (host a `elem` excludeServers)