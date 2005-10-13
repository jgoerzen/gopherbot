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

module RobotsTxt (parseRobots) where

import Text.ParserCombinators.Parsec
import Data.Maybe

eol = string "\n" <|> string "\r\n" -- <|> (eof >> return "")

badline = do many (noneOf "\r\n")
             eol

comment = do char '#'
             many (noneOf "\r\n")
             eol

toeol = do many space
           (try comment) <|> eol

emptyline = toeol

value = many (noneOf " #\t\n\r")

defline key = 
    do k <- string key
       many space
       char ':'
       many space
       v <- value
       toeol
       return v

line key = (try (defline key >>= return . Just ))
       <|> (badline >> return Nothing)

useragent = line "User-agent"
disallow = line "Disallow"

clause = 
    do agents <- many useragent
       disallow <- many disallow
       return (catMaybes agents, catMaybes disallow)

file = many clause

{- | Parse a robots.txt file and return a list corresponding to a clause.
Each tuple in the list contains a list of user agents that the rule applies to,
plus a list of Disallow records. -}
parseRobots :: FilePath -> IO [([String], [String])]
parseRobots fp =
    do r <- parseFromFile file fp
       case r of
              Left x -> do putStrLn $ "WARNING: " ++ (show x)
                           return []
              Right x -> return x
