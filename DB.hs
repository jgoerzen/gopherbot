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

import Types
import Config(numThreads)
import Database.HDBC
import Database.HDBC.PostgreSQL
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
import System.Time
import qualified Data.Map as Map
import qualified DBProcs

dbconnect :: IO Connection
dbconnect = handleSqlError $
    do msg " *** Connecting to DB"
       connectPostgreSQL ""

{- | Initialize the database system. -}
initdb :: IO ()
initdb =
    do msg " *** Initializing database system..."
       handleSqlError $
         do c <- dbconnect
            initTables c
            r <- getCount c "state = ?" [toSql VisitingNow]
            run c "SET ENABLE_SEQSCAN to OFF" []
            when (r > 0) (msg $ "Resetting " ++ (show r) ++ 
                              " files from VisitingNow to NotVisited.")
            run c "UPDATE FILES SET STATE = ? WHERE state = ?"
                  [toSql NotVisited, toSql VisitingNow] 
            commit c
            disconnect c

initTables conn = handleSqlError $
    do t <- getTables conn
       let t2 = map (map toUpper) t
       if not (elem "FILES" t2)
          then do run conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT, timestamp INTEGER, log TEXT)" []
                  run conn "CREATE UNIQUE INDEX files1 ON files(host, port, path)" []
                  --execute conn "CREATE INDEX files2 ON files(host, port)"
                  run conn "CREATE INDEX filesstate ON files (state, host)" []
                  --execute conn "CREATE INDEX files3 ON files(host)"
                  --execute conn "CREATE INDEX files4 ON files(host, state)"
                  mapM_ (\c -> run conn c []) DBProcs.funcs
          else return ()

noteErrorOnHost :: Lock -> GASupply -> Connection -> String -> String -> IO ()
noteErrorOnHost l gasupply c h log = handleSqlError $
     do t <- myThreadId
        modifyMVar gasupply (\m -> bracket_ (acquire l) (release l) $
            do ti <- now
               msg $ " *** Noting error on host " ++ h
               case Map.lookup t m of
                 Nothing -> return ()
                 Just (h, sth) -> finish sth
               run c ("UPDATE FILES SET state = ?, log = ?, timestamp = ?"
                      ++ " WHERE host = ? AND (state = ? OR state = ?)")
                       [toSql ErrorState, toSql log, ti,
                        toSql h, toSql NotVisited, toSql VisitingNow]
               return (Map.delete t m, ())
                        )

updateItem :: Lock -> Connection -> GAddress -> State -> String -> IO ()
updateItem lock conn g s log = withLock lock $ updateItemNL conn g s log

updateItemNL :: Connection -> GAddress -> State -> String -> IO ()
updateItemNL conn g s log = handleSqlError $ withTransaction conn (\c ->
                                             updateItemNLNT c g s log)

mergeItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
mergeItemNLNT conn g s log =
    do t <- now
       run conn "SELECT merge_files (?, ?, ?, ?, ?, ?, ?)"
                 [toSql (host g), toSql (port g), toSql (dtype g),
                  toSql (path g), toSql s, t, toSql log]
       return ()

queueItemNLNT conn g s log =
    do t <- now
       run conn "SELECT queue_files (?, ?, ?, ?, ?, ?, ?)"
           [toSql (host g), toSql (port g), toSql (dtype g),
            toSql (path g), toSql s, t, toSql log]
       return ()

updateItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
updateItemNLNT c g s log = mergeItemNLNT c g s log
                   
now = do c <- getClockTime
         return $ toSql ((\(TOD x _) -> x) c)

getCount :: Connection -> String -> [SqlValue] -> IO Integer
getCount conn whereclause parms =
    do r <- quickQuery conn 
            ("SELECT COUNT(*) FROM FILES WHERE " ++ whereclause) parms
       return . fromSql . head . head $ r

queueItem :: Lock -> Connection -> GAddress -> IO ()
queueItem lock conn g = withLock lock $ 
                        withTransaction conn (\c -> queueItemNL conn g)

queueItems :: Lock -> Connection -> [GAddress] -> IO ()
queueItems lock conn g = withLock lock $ withTransaction conn 
                         (\c -> mapM_ (queueItemNL c) g)
                                                       
-- Don't care if the insert fails; that means we already know of it.      
queueItemNL :: Connection -> GAddress -> IO ()
queueItemNL conn g = handleSqlError $
    queueItemNLNT conn g NotVisited ""

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $
    getCount conn "state = ?" [toSql NotVisited]

{- | Gets the next item to visit, if any, and sets the status
to Visiting.  Returns Nothing if there is no next item.

General algorithm: pick an available host (one not being serviced by
another thread) and process everything possible within it.
-}
popItem :: Lock -> GASupply -> Connection -> IO (Maybe GAddress)
popItem lock gamv conn = handleSqlError $ threadDelay 1000000 >> 
    do t <- myThreadId
       modifyMVar gamv (\map -> bracket_ (acquire lock) (release lock) $
         case Map.lookup t map of
           Nothing -> beginSearch map conn
           Just (host, sth) -> fetchSth map (host, sth) conn
                       )

{- Begin the search for new selectors.  We can assume that this
thread is not in the map, since this function is only called in that case. -}
beginSearch :: Map.Map ThreadId (String, Statement) -> Connection -> IO (Map.Map ThreadId (String, Statement), Maybe GAddress)
beginSearch m conn = handleSqlError  $
    do sth <- prepare conn ("SELECT host FROM files WHERE state = ? " ++ 
                            whereclause ++ " LIMIT 1")
       execute sth ((toSql NotVisited):params)
       t <- myThreadId
       r <- fetchRow sth
       finish sth
       case r of
         Just [h] ->
               do newsth <- prepare conn 
                            "SELECT * FROM files WHERE state = ? AND host = ?\
                              \ LIMIT 2000"
                  execute newsth [toSql NotVisited, h]
                  let newmap = Map.insert t (fromSql h, newsth) m
                  fetchSth newmap (fromSql h, newsth) conn
         Nothing ->
               do msg "No available hosts; dying"
                  -- Couldn't find any available hosts.  For now, we just die.
                  -- FIXME: later should find a better way to do this.
                  return (m, Nothing)
         x -> fail $ "Unexpected result in beginSearch: " ++ show x
    where whereclause = 
              case map fst (Map.elems m) of
                [] -> ""
                x -> " AND " ++ (concat . intersperse " AND " . 
                                 map (\y -> " host != ?") $ x)
          params = map (toSql . fst) (Map.elems m)

fetchSth :: Map.Map ThreadId (String, Statement) -> (String, Statement) 
         -> Connection
         -> IO (Map.Map ThreadId (String, Statement), Maybe GAddress)
fetchSth m (host, sth) conn =
    do r <- fetchRow sth
       case r of
         Just row ->
             return (m, Just (GAddress {host = fromSql (head row),
                                        port = fromSql (row !! 1),
                                        dtype = fromSql (row !! 2),
                                        path = fromSql (row !! 3)}))
         Nothing -> do t <- myThreadId
                       finish sth
                       beginSearch (Map.delete t m) conn

