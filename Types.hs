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

module Types where
import Control.Concurrent(ThreadId)
import Control.Concurrent.MVar(MVar)
import Data.List
import Data.Map(Map)
import Database.HDBC

type GASupply = MVar (Map ThreadId (String, Statement))

data State = NotVisited | VisitingNow | Visited | ErrorState | Excluded
    deriving (Eq, Read, Show)

instance SqlType State where
    toSql s = SqlString (show s)
    fromSql (SqlString s) = read s
    fromSql x = error $ "Cannot convert " ++ show x ++ " into gopherbot.State"

data GAddress = GAddress {host :: String, port :: Int, dtype :: Char,
                          path :: String}
    deriving (Eq)

instance Show GAddress where
    show a = concat . intersperse ":" $
             [host a, show (port a), [dtype a], path a]

type Lock = MVar ThreadId

