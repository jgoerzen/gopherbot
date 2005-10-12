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

initdb :: IO Connection

initdb =
    do putStrLn " *** Initializing database system..."
       handleSqlError $
         do c <- connect ("state.sql3") ReadWriteMode
            initTables c
            return c

initTables conn = handleSqlError $
    do t <- tables conn
       let t2 = map (map toUpper) t
       if not (elem "FILES" t2)
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT)"
                  execute conn "CREATE UNIQUE INDEX files1 ON files(host, port, dtype, path, state)"
                  execute conn "CREATE INDEX files2 ON files(host, port)"
          else return ()

matchClause :: GAddress -> String
matchClause g =
    "host = " 
    ++ toSqlValue (host g) ++ " AND port = " 
    ++ toSqlValue (port g) ++ " AND path = "
    ++ toSqlValue (path g) ++ " AND dtype = "
    ++ toSqlValue [dtype g]

updateItem :: Connection -> GAddress -> State -> IO ()
updateItem conn g s = handleSqlError $ inTransaction conn (\c ->
    do execute c $ "DELETE FROM files WHERE " ++ matchClause g
       execute c $ "INSERT INTO files VALUES (" ++
           toSqlValue (host g) ++ ", " ++
           toSqlValue (port g) ++ ", " ++
           toSqlValue [dtype g] ++ ", " ++
           toSqlValue (path g) ++ ", " ++
           toSqlValue (show s) ++ ")"
                      )

queueItem :: Connection -> GAddress -> IO ()
queueItem conn g = handleSqlError $
    do sth <- query conn $ "SELECT COUNT(*) FROM FILES WHERE " ++ matchClause g
       h <- fetch sth
       (r::Int) <- getFieldValue sth "COUNT(*)"
       --putStrLn (show r)
       if r == 0
          then updateItem conn g NotVisited
          else return ()

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $
    do sth <- query conn $ "SELECT COUNT(*) FROM FILES WHERE state = " ++
                           (toSqlValue (show NotVisited))
       h <- fetch sth
       r <- getFieldValue sth "COUNT(*)"
       closeStatement sth
       return r

-- | Gets the next item to visit, if any, and sets the status
-- to Visiting.  Returns Nothing if there is no next item.
popItem :: Connection -> IO (Maybe GAddress)
popItem conn =
    do sth <- query conn $ "SELECT * FROM files WHERE state = " ++
                           (toSqlValue (show NotVisited))
                           ++ " LIMIT 1"
       h <- fetch sth
       if h
          then do h <- getFieldValue sth "host"
                  p <- getFieldValue sth "port"
                  pa <- getFieldValue sth "path"
                  dt <- getFieldValue sth "dtype"
                  let po = read p
                  let ga = GAddress {host = h, port = po, path = pa, dtype = head dt}
                  closeStatement sth
                  updateItem conn ga VisitingNow
                  return (Just ga)
          else do closeStatement sth
                  return Nothing

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)
