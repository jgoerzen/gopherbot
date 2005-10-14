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
import MissingH.Threads.Timeout

timeo = 45 * 1000000
cto :: String -> IO a -> IO a
cto msg action =
    do r <- timeout timeo action
       case r of
              Nothing -> fail msg
              Just x -> return x

dlItem :: GAddress -> FilePath -> IO ()
dlItem ga fp =
    do s <- cto "Timeout on connect" $ 
            connectTCP (host ga) (fromIntegral . port $ ga)
       cto "Timeout on send" $ sendAll s $ (path ga) ++ "\r\n"
       cto "Timeout on shotdown" $ shutdown s ShutdownSend
       if (dtype ga) == '1'
          then dlTillDot s fp
          else dlTo s fp
       cto "Timeout on close" $ sClose s
       
dlTillDot s fp =
    do c <- sGetContents s
       writeFile fp (process c)
    where process :: String -> String
          process = unlines . proc' . lines
          proc' :: [String] -> [String]
          proc' [] = []
          proc' (".":_) = []
          proc' (".\r":_) = []
          proc' (".\r\n":_) = []
          proc' (x:xs) = x : proc' xs

sendAll :: Socket -> String -> IO ()
sendAll s [] = return ()
sendAll s buf =
    do bytessent <- send s buf
       sendAll s (drop bytessent buf)

recvBlocks :: Socket -> (a -> String -> IO a) -> a -> IO a
recvBlocks s action state =
    do buf <- cto "Timeout on recv" (recv s 8192)
       if buf == []
          then return state
          else do nestate <- action state buf
                  recvBlocks s action newstate

-- FIXME: this is slow and a RAM hog.

sGetContents :: Socket -> IO String
sGetContents s =
    recvBlocks s (\o n -> return $ o ++ n) []

dlTo :: Socket -> FilePath -> IO ()
dlTo s fp =
    do h <- openFile fp WriteMode
       recvBlocks s (\() buf -> hPutStr h buf) ()
       hClose h