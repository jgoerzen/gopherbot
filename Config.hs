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

module Config where

type GAddress = {host :: String, port :: Int, path :: String}
    deriving (Eq, Show)

data State = NotVisited | VisitingNow | Visited
    deriving (Eq, Read, Show)

startingURLs = ["gopher://home.jumpjet.info/11/Gopher_Jewels_2"]
excludeServers ["gopher.quux.org", "quux.org"]
baseDir = "/home/jgoerzen/gopher-arch"
