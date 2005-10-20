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
import Database.HSQL
import Database.HSQL.PostgreSQL
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

{- | Initialize the database system. -}
initdb :: IO Connection
initdb =
    do msg " *** Initializing database system..."
       handleSqlError $
         do c <- connect "" "" "" ""
            initTables c
            r <- getCount c $ "state = " ++ ce (show VisitingNow)
            execute c "SET ENABLE_SEQSCAN to OFF"
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
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT, timestamp INTEGER, log TEXT)"
                  execute conn "CREATE UNIQUE INDEX files1 ON files(host, port, path)"
                  --execute conn "CREATE INDEX files2 ON files(host, port)"
                  execute conn "CREATE INDEX filesstate ON files (state, host)"
                  --execute conn "CREATE INDEX files3 ON files(host)"
                  --execute conn "CREATE INDEX files4 ON files(host, state)"
                  mapM_ (execute conn) DBProcs.funcs
          else return ()

matchClause :: GAddress -> String
matchClause g =
    "host = " 
    ++ ce (host g) ++ " AND port = " 
    ++ toSqlValue (port g) ++ " AND path = "
    ++ ce (path g) ++ " AND dtype = "
    ++ toSqlValue [dtype g]

noteErrorOnHost :: Lock -> GASupply -> Connection -> String -> String -> IO ()
noteErrorOnHost l gasupply c h log = handleSqlError $
     do t <- myThreadId
        modifyMVar gasupply (\m -> bracket_ (acquire l) (release l) $
            do ti <- now
               case Map.lookup t m of
                 Nothing -> return ()
                 Just (h, sth) -> closeStatement sth
               execute c $ "UPDATE FILES SET state = " ++
                 ce (show ErrorState) ++
                 ", log = " ++ ce log ++ ", timestamp = " ++ ti ++
                 " WHERE host = " ++ ce h
               return (Map.delete t m, ())
                        )

updateItem :: Lock -> Connection -> GAddress -> State -> String -> IO ()
updateItem lock conn g s log = withLock lock $ updateItemNL conn g s log

updateItemNL :: Connection -> GAddress -> State -> String -> IO ()
updateItemNL conn g s log = handleSqlError $ inTransaction conn (\c ->
                                             updateItemNLNT c g s log)

mergeItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
mergeItemNLNT conn g s log =
    do t <- now
       execute conn $ "SELECT merge_files (" ++
              ce (host g) ++ ", " ++
              toSqlValue (port g) ++ ", " ++
              ce [dtype g] ++ ", " ++
              ce (path g) ++ ", " ++
              ce (show s) ++ ", " ++
              t ++ ", " ++ ce log ++ ")"

queueItemNLNT conn g s log =
    do t <- now
       execute conn $ "SELECT queue_files (" ++
              ce (host g) ++ ", " ++
              toSqlValue (port g) ++ ", " ++
              ce [dtype g] ++ ", " ++
              ce (path g) ++ ", " ++
              ce (show s) ++ ", " ++
              t ++ ", " ++ ce log ++ ")"

updateItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
updateItemNLNT c g s log = mergeItemNLNT c g s log
                   
now = do c <- getClockTime
         return $ toSqlValue ((\(TOD x _) -> x) c)

getCount :: Connection -> String -> IO Integer
getCount conn whereclause =
    do sth <- query conn $ "SELECT COUNT(*) FROM FILES WHERE " ++ whereclause
       h <- fetch sth
       r <- getFieldValue sth "count"
       closeStatement sth
       return r

queueItem :: Lock -> Connection -> GAddress -> IO ()
queueItem lock conn g = withLock lock $ 
                        inTransaction conn (\c -> queueItemNL conn g)

queueItems :: Lock -> Connection -> [GAddress] -> IO ()
queueItems lock conn g = withLock lock $ inTransaction conn 
                         (\c -> mapM_ (queueItemNL c) g)
                                                       
-- Don't care if the insert fails; that means we already know of it.      
queueItemNL :: Connection -> GAddress -> IO ()
queueItemNL conn g = handleSqlError $
    queueItemNLNT conn g NotVisited ""

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $
    getCount conn $ "state = " ++ (toSqlValue (show NotVisited))

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
beginSearch m conn = handleSqlError  $
    do sth <- query conn $ "SELECT host FROM files WHERE state = " ++
                           (toSqlValue $ show NotVisited) ++ " "
                           ++ whereclause
                           ++ " LIMIT 1"
       t <- myThreadId
       r <- fetch sth
       if r
          then do h <- getFieldValue sth "host"
                  closeStatement sth
                  newsth <- query conn $ "SELECT * FROM files WHERE state = " 
                                       ++ (toSqlValue $ show NotVisited) ++
                                       " AND host = " ++ ce h
                  let newmap = Map.insert t (h, newsth) m
                  fetchSth newmap (h, newsth) conn
          else do closeStatement sth
                  msg "No available hosts; dying"
                  -- Couldn't find any available hosts.  For now, we just die.
                  -- FIXME: later should find a better way to do this.
                  return (m, Nothing)
    where whereclause = 
              case map fst (Map.elems m) of
                [] -> ""
                x -> " AND " ++ (concat . intersperse " AND " . 
                                 map (\y -> " host != " ++ ce y) $ x)

fetchSth m (host, sth) conn =
    do r <- fetch sth
       if r
          then do h <- getFieldValue sth "host"
                  p <- getFieldValue sth "port"
                  pa <- getFieldValue sth "path"
                  dt <- getFieldValue sth "dtype"
                  let po = read p
                  let ga = GAddress {host = h, port = po, 
                                     path = pa, dtype = head dt}
                  return (m, Just ga)

          else do t <- myThreadId
                  closeStatement sth
                  beginSearch (Map.delete t m) conn

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

