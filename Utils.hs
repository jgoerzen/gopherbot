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

module Utils where

import Types
import Config
import MissingH.Maybe
import MissingH.Path
import Control.Concurrent
import Control.Exception
import System.IO
--import Foreign.C.String
import Data.List


getFSPath :: GAddress -> FilePath
getFSPath ga =
    forceMaybeMsg ("getFSPath1 " ++ show ga) . secureAbsNormPath (baseDir ++ "/gopher") $ base
    where base = (host ga) ++ "/" ++ (show $ port ga) ++ "/" ++
              (path ga) ++ case (dtype ga) of
                                           '1' -> "/.gophermap"
                                           _ -> ""

newLock :: IO Lock
newLock = newEmptyMVar

acquire :: Lock -> IO ()
acquire l =
    do t <- myThreadId
       putMVar l t

release :: Lock -> IO ()
release l =
    do t <- myThreadId
       r <- tryTakeMVar l
       case r of
              Nothing -> do msg $ "Warning: released lock which was unheld."
              Just x -> if x == t
                            then return ()
                            else fail $ "Thread " ++ (show t) ++
                                        " released lock held by thread " ++
                                        (show x)

withLock :: Lock -> (IO a) -> IO a
withLock l action = bracket_ (acquire l) (release l) action

msg :: String -> IO ()
msg l =
    do t <- myThreadId
       let disp = (show t) ++ ": " ++ l ++ "\n"
       putStr disp
       hFlush stdout
       --withCStringLen disp (\(c, len) -> hPutBuf stdout c len >> hFlush stdout)

ce :: String -> String
ce i =
    '\'' : 
         (concat $ map (\c -> if c == '\'' then "''" else [c]) i)
    ++ "'"
