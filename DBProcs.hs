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

module DBProcs(funcs) where
import Data.List

funcs :: [String]
funcs = [mergefunc, queuefunc]

mergefunc = concat . intersperse "\n" $
 ["CREATE FUNCTION merge_files(h TEXT, p INTEGER, dt TEXT, pa TEXT, s TEXT, ts INTEGER, l TEXT) RETURNS VOID AS",
 "$$",
 "BEGIN",
 "    LOOP",
 "        UPDATE db SET dtype = dt, state = s, timestampe = ts, log = l",
 "               WHERE host = h AND port = p AND path = pa;",
 "        IF found THEN",
 "            RETURN;",
 "        END IF;",
 "",
 "        BEGIN",
 "            INSERT INTO files(host,port,dtype,path,state,timestamp,log) VALUES ",
 "               (h, p, dt, pa, s, ts, l);",
 "            RETURN;",
 "        EXCEPTION WHEN unique_violation THEN",
 "            -- do nothing",
 "        END;",
 "    END LOOP;",
 "END;",
 "$$",
 "LANGUAGE plpgsql"]

queuefunc = concat . intersperse "\n" $
 ["CREATE FUNCTION queue_files(h TEXT, p INTEGER, dt TEXT, pa TEXT, s TEXT, ts INTEGER, l TEXT) RETURNS VOID AS",
 "$$",
 "BEGIN",
 "            INSERT INTO files(host,port,dtype,path,state,timestamp,log) VALUES ",
 "               (h, p, dt, pa, s, ts, l);",
 "            RETURN;",
 "        EXCEPTION WHEN unique_violation THEN",
 "            RETURN;",
 "END;",
 "$$",
 "LANGUAGE plpgsql"]
