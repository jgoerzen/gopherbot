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
import System.Directory
import DB
import Database.HSQL

main =
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
       putStrLn $ (show n) ++ " items to process"