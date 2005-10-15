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
import System.Time
import qualified Data.Map as Map

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
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT, timestamp INTEGER, log TEXT)"
                  execute conn "CREATE UNIQUE INDEX files1 ON files(host, port, dtype, path, state)"
                  --execute conn "CREATE INDEX files2 ON files(host, port)"
                  execute conn "CREATE INDEX filesstate ON files (state)"
                  execute conn "CREATE INDEX files3 ON files(host)"
                  execute conn "CREATE INDEX files4 ON files(host, state)"
          else return ()

matchClause :: GAddress -> String
matchClause g =
    "host = " 
    ++ ce (host g) ++ " AND port = " 
    ++ toSqlValue (port g) ++ " AND path = "
    ++ ce (path g) ++ " AND dtype = "
    ++ toSqlValue [dtype g]

noteErrorOnHost :: Lock -> Connection -> String -> String -> IO ()
noteErrorOnHost l c h log = withLock l $ handleSqlError $
     do t <- now
        execute c $ "UPDATE FILES SET state = " ++
                 ce (show ErrorState) ++
                 ", log = " ++ ce log ++ ", timestamp = " ++ t ++
                 " WHERE host = " ++ ce h

updateItem :: Lock -> Connection -> GAddress -> State -> String -> IO ()
updateItem lock conn g s log = withLock lock $ updateItemNL conn g s log

updateItemNL :: Connection -> GAddress -> State -> String -> IO ()
updateItemNL conn g s log = handleSqlError $ inTransaction conn (\c ->
                                             updateItemNLNT c g s log)

updateItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
updateItemNLNT c g s log =
    do execute c $ "DELETE FROM files WHERE " ++ matchClause g
       t <- now
       execute c $ "INSERT INTO files VALUES (" ++
           ce (host g) ++ ", " ++
           toSqlValue (port g) ++ ", " ++
           toSqlValue [dtype g] ++ ", " ++
           ce (path g) ++ ", " ++
           toSqlValue (show s) ++ ", " ++
           t ++ ", " ++ ce log ++ ")"
                   
now = do c <- getClockTime
         return $ toSqlValue ((\(TOD x _) -> x) c)

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
          then updateItemNLNT conn g NotVisited ""
          else return ()

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $
    getCount conn $ "state = " ++ (toSqlValue (show NotVisited))

{- | Gets the next item to visit, if any, and sets the status
to Visiting.  Returns Nothing if there is no next item.

General algorithm: get a list of the top (15*numThreads) eligible hosts,
then pick one. -}
popItem :: Lock -> GASupply -> Connection -> IO (Maybe GAddress)
popItem lock gamv conn = 
    do r <- handleSqlError $ modifyMVar gamv $
            fixHL (popItem' lock gamv conn False False)
       case r of
              StartOver -> do threadDelay (30 * 1000000)
                              popItem lock gamv conn
              PIData x -> return (Just x)
              NoData -> return Nothing

fixHL action (hl, x) =
    do t <- myThreadId
       let newhl = Map.delete t hl
       action (newhl, x)

data PIResult = StartOver | PIData GAddress | NoData
popItem' lock gamv conn isNewSth haveRestarted (hl, Nothing) =
    do msg " *** Beginning new iteration"
       sth <- query conn $ 
              "SELECT * FROM files WHERE state = " ++
               (toSqlValue (show NotVisited))
               -- ++ " LIMIT " ++ show (10 * numThreads)
       popItem' lock gamv conn True haveRestarted (hl, Just sth)
popItem' lock gamv conn isNewSth haveRestarted mv@(hl, Just sth) =
    do r <- fetch sth

       case (isNewSth, r) of
         (True, False) -> -- If we just issued a query, and got back zero 
                          -- results, there is no more data to process.
                          do closeStatement sth
                             return $ ((hl, Nothing), NoData)
         (False, False) -> -- Not new query, no rows: start over.
                           -- Sleep if we have already started over this round.
                           do closeStatement sth
                              return ((hl, Nothing), StartOver)
         (_, True) -> -- Have rows.
                      do h <- getFieldValue sth "host"
                         if h `elem` Map.elems hl
                            -- Another thread is watching this.
                            then popItem' lock gamv conn False haveRestarted
                                    (hl, Just sth)
                            else do -- We have a winner!
                                 p <- getFieldValue sth "port"
                                 pa <- getFieldValue sth "path"
                                 dt <- getFieldValue sth "dtype"
                                 let po = read p
                                 let ga = GAddress {host = h, port = po, 
                                                    path = pa, 
                                                    dtype = head dt}
                                 t <- myThreadId
                                 let newhl = Map.insert t h hl
                                 updateItem lock conn ga VisitingNow ""
                                 return $ ((newhl, Just sth), PIData ga)

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

