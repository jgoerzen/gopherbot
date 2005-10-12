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

module NetClient where

import MissingH.Network
import Network.Socket
import System.IO
import Config
import Foreign.C.Types

dlItem :: GAddress -> FilePath -> IO ()
dlItem ga fp =
    do s <- connectTCP (host ga) (fromIntegral . port $ ga)
       setSockOpt s
       h <- socketToHandle s ReadWriteMode
       hSetBinaryMode h True
       hPutStr h $ (path ga) ++ "\r\n"
       hFlush h
       if (dtype ga) == '1'
          then dlTillDot h fp
          else do c <- hGetContents h
                  writeFile fp c
       hClose h
       
dlTillDot h fp =
    do c <- hGetContents h
       writeFile fp (process c)
    where process :: String -> String
          process = unlines . proc' . lines
          proc' :: [String] -> [String]
          proc' [] = []
          proc' (".":_) = []
          proc' (".\r":_) = []
          proc' (".\r\n":_) = []
          proc' (x:xs) = x : proc' xs

setSockOpt :: Socket -> IO ()
setSockOpt s =
    do let fd = fdSocket s
       configsock fd

foreign import ccall unsafe "cutils.h configsock"
  configsock :: CInt -> IO ()
