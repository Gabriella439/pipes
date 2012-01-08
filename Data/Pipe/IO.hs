{-
    Copyright 2012 Gabriel Gonzalez

    This file is part of the Haskell Pipes Library.

    The is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hPDB is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hPDB.  If not, see <http://www.gnu.org/licenses/>.
-}
module Data.Pipe.IO where

import Control.Monad
import Control.Monad.Trans
import Data.Pipe

printer :: (Show a) => Consumer a IO b
printer = forever $ await >>= lift . print
