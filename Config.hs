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
import Data.List
import Types

startingAddresses :: [GAddress]
startingAddresses = 
    [GAddress {host="home.jumpjet.info",port= 70,dtype= '1',path= "1/Gopher_Jewels_2"}]
excludeServers = [] --["gopher.quux.org", "quux.org"]
baseDir = "/home/jgoerzen/tree/gopher-arch"

numThreads = 20


