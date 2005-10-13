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

module RobotsTxt  where

-- FIXME: should only consider first user-agent match?

import Text.ParserCombinators.Parsec
import Data.Maybe
import MissingH.Str
import Network.URI

ws = many (oneOf " \v\f\t")

eol = string "\n" <|> string "\r\n" -- <|> (eof >> return "")

badline = do many (noneOf "\r\n")
             eol

comment = do char '#'
             many (noneOf "\r\n")
             eol

toeol = do ws
           eol <|> comment

emptyline = (try comment) <|> toeol

value = many (noneOf "#\t\n\r")

defline key = 
    do string key
       kv

kv = do ws
        char ':'
        ws
        v <- value
        toeol
        return (rstrip v)

line key = (try (defline key))
           <|> (kv >> fail "foo")
           <|> (emptyline >> line key)

useragent = try (line "User-agent")
disallow = try (line "Disallow")

clauses = 
    do agents <- many useragent
       disallow <- many disallow
       if agents == []
          then fail "Found no User-agent"
          else do next <- ((try clauses) <|> (return []))
                  return ((agents, disallow) : next)

{- | Parse a robots.txt file and return a list corresponding to a clause.
Each tuple in the list contains a list of user agents that the rule applies to,
plus a list of Disallow records. -}
parseRobots :: FilePath -> IO [([String], [String])]
parseRobots fp =
    do r <- parseFromFile clauses fp
       case r of
              Left x -> return []
              Right x -> return x

{- | Given a parsed file, a user agent, and a URL, determine whether
it's OK to process that URL. -}
isURLAllowed :: [([String], [String])] -> String -> String -> Bool
isURLAllowed parsed agent url =
    let agentsfiltered = filter (\i -> "*" `elem` (fst i) ||
                               agent `elem` (fst i)) parsed
        disallowparts = concat . map snd $ agentsfiltered
        escapedurl = escapeURIString (\c -> not $ elem c " ?\n\r\0&") url
        in
        not (any (\i -> startswith i url || startswith i escapedurl) disallowparts)
        
