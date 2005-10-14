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

module DB where

import Config
import Database.HSQL
import Database.HSQL.SQLite3
import Data.Char
import System.IO
import Data.List
import Utils
import Control.Exception
import Control.Monad(when, replicateM_)
import Control.Concurrent.MVar
import Control.Concurrent
import Data.HashTable as HT
import MissingH.Maybe

{- | Initialize the database system. -}
initdb :: IO Connection
initdb =
    do msg " *** Initializing database system..."
       handleSqlError $
         do c <- connect ("state.sql3") ReadWriteMode
            execute c "pragma synchronous = off"
            execute c "pragma temp_store = memory"
            execute c "pragma cache_size = 60000"
            initTables c
            r <- getCount c $ "state = " ++ ce (show VisitingNow)
            when (r > 0) (msg $ "Resetting " ++ (show r) ++ 
                              " files from VisitingNow to NotVisited.")
            execute c $ "UPDATE FILES SET STATE = " ++
                    ce (show NotVisited) ++ " WHERE state = " ++
                    ce (show VisitingNow)
            return c

initTables conn = handleSqlError $
    do t <- tables conn
       let t2 = map (map toUpper) t
       if not (elem "FILES" t2)
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT)"
                  execute conn "CREATE UNIQUE INDEX files1 ON files(host, port, dtype, path, state)"
                  --execute conn "CREATE INDEX files2 ON files(host, port)"
                  execute conn "CREATE INDEX filesstate ON files (state)"
                  execute conn "CREATE INDEX files3 ON files(host)"
          else return ()

matchClause :: GAddress -> String
matchClause g =
    "host = " 
    ++ ce (host g) ++ " AND port = " 
    ++ toSqlValue (port g) ++ " AND path = "
    ++ ce (path g) ++ " AND dtype = "
    ++ toSqlValue [dtype g]

noteErrorOnHost :: Lock -> Connection -> String -> IO ()
noteErrorOnHost l c h = withLock l $ handleSqlError $
     execute c $ "UPDATE FILES SET state = " ++
                 ce (show ErrorState) ++
                 " WHERE host = " ++ ce h

updateItem :: Lock -> Connection -> GAddress -> State -> IO ()
updateItem lock conn g s = withLock lock $ updateItemNL conn g s

updateItemNL :: Connection -> GAddress -> State -> IO ()
updateItemNL conn g s = handleSqlError $ inTransaction conn (\c ->
                                     updateItemNLNT c g s)

updateItemNLNT :: Connection -> GAddress -> State -> IO ()
updateItemNLNT c g s =
    do execute c $ "DELETE FROM files WHERE " ++ matchClause g
       execute c $ "INSERT INTO files VALUES (" ++
           ce (host g) ++ ", " ++
           toSqlValue (port g) ++ ", " ++
           toSqlValue [dtype g] ++ ", " ++
           ce (path g) ++ ", " ++
           toSqlValue (show s) ++ ")"


getCount :: Connection -> String -> IO Integer
getCount conn whereclause =
    do sth <- query conn $ "SELECT COUNT(*) FROM FILES WHERE " ++ whereclause
       h <- fetch sth
       r <- getFieldValue sth "COUNT(*)"
       closeStatement sth
       return r

queueItem :: Lock -> Connection -> GAddress -> IO ()
queueItem lock conn g = withLock lock $ 
                        inTransaction conn (\c -> queueItemNL conn g)

queueItems :: Lock -> Connection -> [GAddress] -> IO ()
queueItems lock conn g = withLock lock $ inTransaction conn (\c ->
    mapM_ (queueItemNL c) g
                                                            )
                                                             
queueItemNL :: Connection -> GAddress -> IO ()
queueItemNL conn g = handleSqlError $
    do r <- getCount conn (matchClause g)
       if r == 0
          then updateItemNLNT conn g NotVisited
          else return ()

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $
    getCount conn $ "state = " ++ (toSqlValue (show NotVisited))

{- | Gets the next item to visit, if any, and sets the status
to Visiting.  Returns Nothing if there is no next item.

General algorithm: get a list of the top (15*numThreads) eligible hosts,
then pick one. -}
popItem :: Lock -> MVar (Maybe GAddress) -> Connection -> IO (Maybe GAddress)
popItem lock gasupply conn = withLock lock $ handleSqlError $
    do newga <- takeMVar gasupply
       case newga of
               Nothing -> return Nothing
               Just ga -> do updateItemNL conn ga VisitingNow
                             --addHost hosts (host ga)
                             return (Just ga)

{- | Send new GAddress objects to the specified mvar. 
Run forever. -}
nextFinder :: MVar (Maybe GAddress) -> Connection -> [String] -> IO ()
nextFinder mv conn recent =
    do sth <- query conn $ "SELECT * FROM files WHERE state = " ++
                         (toSqlValue (show NotVisited)) ++ " " ++
                         (whereclause recent)
                         ++ " LIMIT 1"
                         -- ++ " LIMIT " ++ show (10 * numThreads)
       r <- fetch sth
       if r
          then do h <- getFieldValue sth "host"
                  p <- getFieldValue sth "port"
                  pa <- getFieldValue sth "path"
                  dt <- getFieldValue sth "dtype"
                  let po = read p
                  let ga = GAddress {host = h, port = po, path = pa, 
                                     dtype = head dt}
                  let recent' = take memorysize (h : recent)
                  closeStatement sth
                  putMVar mv (Just ga)
                  --performGC
                  nextFinder mv conn recent'
          else do closeStatement sth
                  n <- getCount conn $ "state = " ++ 
                       toSqlValue (show NotVisited)
                  if n == 0
                     -- Didn't find anything, so we send the shutdown 
                     -- message (Nothing)
                     -- all over the place.
                     then replicateM_ (fromIntegral numThreads)
                              (putMVar mv Nothing)
                     -- Didn't find anything because of recent list,
                     -- but there are records.  Wait a bit and then try
                     -- again.
                     else do threadDelay (30 * 1000000)
                             nextFinder mv conn recent
    where memorysize = (fromIntegral (numThreads - 1))::Int
          whereclause [] = ""
          whereclause rl = " AND " ++ 
                           (concat . intersperse " AND " .
                            map (\i -> " (host != " ++ (ce i) ++ ") ") $ rl
                           )
          

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

