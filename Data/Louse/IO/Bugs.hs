-- louse - distributed bugtracker
-- Copyright (C) 2015 Peter Harpending
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Data.Louse.IO.Bugs
-- Description : Stuff for reading & writing bugs
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.IO.Bugs where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.ByteString.Char8 as Bsc
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary
import Data.Louse.Types
import Data.Louse.IO.DataFiles (_bugs_dir)
import Data.Louse.Trivia (randomIdent)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import System.Directory (removeFile)

-- For reference:
-- 
-- -- |A bug.
-- data Bug = Bug { bugReporter :: Maybe Person
--                , bugCreationDate :: UTCTime
--                , bugTitle :: T.Text
--                , bugDescription :: T.Text
--                , bugOpen :: Bool
--                , bugComments :: [Comment]
--                }
--   deriving (Eq, Show)


-- |Add a bug to the current project. This doesn't return a bug. It
-- instead writes the bug to a file, and returns the 'BugId' pertaining
-- to the file.
addBug :: Maybe Person   -- ^The reporter
       -> T.Text         -- ^Title of the bug
       -> T.Text         -- ^Description of the bug
       -> IO BugId       -- ^Resulting 'BugId'
addBug person title description =
  do reportTime <- getCurrentTime
     let bugIsOpen = False
         comments = mempty
         newBug =
           Bug person reportTime title description bugIsOpen comments
     bugid <- fmap Bsc.unpack randomIdent
     runResourceT
       (connect (sourceLbs (encode newBug))
                (sinkFile (mconcat [_bugs_dir,bugid,".yaml"])))
     return (T.pack bugid)

-- |Close a bug. This actually edits the files, so be careful.
closeBug :: BugId -> IO ()
closeBug bugid =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".yaml"])
     bugAST <-
       runResourceT
         (connect (sourceFile bugsPath)
                  (sinkParser json))
     bug <-
       case fromJSON bugAST of
         Success b -> pure b
         Error s -> fail s
     runResourceT
       (connect (sourceLbs (encode (bug {bugOpen = False})))
                (sinkFile bugsPath))

-- |Comment on a bug. This actually edits the data files, so be careful!
commentOnBug :: BugId                      -- ^The bug on which to comment
             -> Maybe PersonId             -- ^The commenter
             -> T.Text                     -- ^The actual comment text
             -> IO ()
commentOnBug bugid personid comment =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".yaml"])
     bugAST <-
       runResourceT
         (connect (sourceFile bugsPath)
                  (sinkParser json))
     bug <-
       case fromJSON bugAST of
         Success b -> pure b
         Error s -> fail s
     commentTime <- getCurrentTime
     let newComment =
           Comment personid commentTime comment
     runResourceT
       (connect (sourceLbs (encode (bug {bugComments =
                                           (mappend (bugComments bug)
                                                    [newComment])})))
                (sinkFile bugsPath))

-- ^Delete a bug from the list of bugs. 
deleteBug :: BugId -> IO ()
deleteBug bugid =
  let bugPath =
        (mconcat [_bugs_dir,T.unpack bugid,".yaml"])
  in removeFile bugPath