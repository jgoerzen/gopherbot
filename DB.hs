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
import Types
import Data.List

initdb :: IO Connection

initdb =
    do putStrLn " *** Initializing database system..."
       handleSqlError $
         do c <- connect ("state.sql3") ReadWriteMode
            initTable c
            return c

initTables conn = handleSqlError $
    do t <- table conn
       let t2 = map (map toUpper) t
       if not (elem "FILES" tw)
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER,  path TEXT, state TEXT)"
                  execute conn "CREATE UNIQUE INDEX pending1 ON pending(host, port, path, state)"
                  execute conn "CREATE INDEX pending2 ON pending(host, port)"
          else return ()

updateItem :: Connection -> GAddress -> State -> IO ()
updateItem conn g s = handleSqlError $
    do execute conn $ "DELETE FROM files WHERE host = " 
        ++ toSqlValue (host g) ++ " AND port = " 
        ++ toSqlValue (port g) ++ " AND path = "
        ++ toSqlValue (path g)
       execute conn $ "INSERT INTO files VALUES (" ++
           toSqlValue (host g) ++ ", " ++
           toSqlValue (port g) ++ ", " ++
           toSqlValue (path g) ++ ", " ++
           toSqlValue (show s) ++ ")"

-- | Gets the next item to visit, if any, and sets the status
-- to Visiting.  Returns Nothing if there is no next item.
popItem