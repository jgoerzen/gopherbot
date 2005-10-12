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

module DirParser (parseGMap) where

import Config
import Text.ParserCombinators.Parsec

parseGMap :: FilePath -> IO [GAddress]
parseGMap fp =
    do r <- parseFromFile parserMain fp
       case r of
              Left x -> do putStrLn $ "WARNING: " ++ (show x)
                           return []
              Right x -> return x

parserMain = many gmapline

eol = string "\r\n" <|> string "\n"
extratoeol = do many (noneOf "\r\n")
                (eof >> return "") <|> eol
                
field = many (noneOf "\r\n\t")

gmapline = 
    do dtype <- anyChar
       name <- field
       tab
       path <- field
       tab
       host <- field
       tab
       port <- field
       extratoeol
       return GAddress {host = host, port = read port, dtype = dtype,
                        path = path}
