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
-- Module      : Data.Louse.IO.Read
-- Description : 'readLouse' and Friends
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.IO.Read where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import           Data.Louse.IO.DataFiles
import           Data.Monoid
import qualified Data.Map as M
import           Data.List.Utils (split)
import           Data.Louse.Types
import           Data.Ratio ((%))
import qualified Data.Text as T
import           Safe
import           System.Directory
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error
import           Text.Editor

-- |Read the 'Louse' from the current directory
-- 
-- > readLouse = readLouseFrom =<< getCurrentDirectory
-- 
readLouse :: IO (Either String Louse)
readLouse = readLouseFrom =<< getCurrentDirectory

-- |Read the 'Louse' from the current directory
-- 
-- > readLouseMay = readLouseFromMay =<< getCurrentDirectory
-- 
readLouseMay :: IO (Maybe Louse)
readLouseMay = readLouseFromMay =<< getCurrentDirectory

-- |Read the 'Louse' from the current directory
-- 
-- > readLouseErr = readLouseFromErr =<< getCurrentDirectory
-- 
readLouseErr :: IO Louse
readLouseErr = readLouseFromErr =<< getCurrentDirectory

-- |Wrapper around 'readLouseFromErr', which catches errors, and returns
-- a 'Left' if there is an error.
readLouseFrom 
  :: FilePath                   -- ^The working directory
  -> IO (Either String Louse)
readLouseFrom fp = (try (readLouseFromErr fp) :: IO (Either SomeException Louse)) >>= \case
                     Left err -> pure (Left (show err))
                     Right x  -> pure (Right x)

-- |Wrapper around 'readLouseFromErr', which returns 'Nothing' if there
-- is an error.
readLouseFromMay 
  :: FilePath         -- ^The working directory
  -> IO (Maybe Louse)
readLouseFromMay = readLouseFrom >=> \case
                     Left _  -> pure Nothing
                     Right x -> pure (Just x)

-- |This is a function to read the Louse instance from a directory.
readLouseFromErr 
  :: FilePath -- ^The path to the project directory (i.e. NOT .louse)
  -> IO Louse -- ^The resulting 'Louse'
readLouseFromErr fp =
  do let prjson = fp <> _project_json
     prjInfoExists <- doesFileExist prjson
     prjInfoBS <-
       if prjInfoExists
          then fmap Just (Bl.readFile prjson)
          else pure Nothing
     prjInfo <-
       case fmap eitherDecode prjInfoBS of
         Nothing -> pure Nothing
         Just x ->
           case x of
             Left err ->
               fail (mconcat ["JSON decoding of "
                             ,prjson
                             ," failed with: "
                             ,"\n"
                             ,err])
             Right pi -> pure (Just pi)
     fmap (Louse fp prjInfo)
          (readBugsFromErr fp)

-- |Lazily reads the bugs.
readBugsFromErr 
  :: FilePath             -- ^The path to the project directory
  -> IO (M.Map BugId Bug) -- ^The resulting Map
readBugsFromErr fp = 
  readFilesFromErr $ mappend fp _bugs_dir

-- |Lazily reads files in a directory, returns a 'M.Map' of the name
-- of the file, along with the decoded value.
readFilesFromErr :: FromJSON t
                 => FilePath -> IO (IdMap t)
readFilesFromErr directoryPath =
  -- Get the files in the directory
  do filePaths <- getDirectoryContents directoryPath
     -- For each filePath
     fmap
       M.fromList
       (forM filePaths
             (\fp ->
                do fileBytes <- Bs.readFile fp
                   decodedContents <-
                     case decodeStrict fileBytes of
                       Nothing ->
                         fail (mappend "Could not decode contents of: " fp)
                       Just v -> pure v
                   let fileName =
                         T.pack (reverse (drop 5 (reverse fp)))
                   return (fileName,decodedContents)))

-- |Look up a bug by its 'BugId'
lookupBug :: Louse -> BugId -> Maybe Bug
lookupBug louse bugid =
  M.lookup bugid (louseBugs louse)

-- |Get the status
statusStr :: FilePath -> IO String
statusStr dir =
  do let errprint = hPutStrLn stderr
         rbind = (>>=)
     louse <-
       rbind (tryIOError (readLouseFromErr dir))
             (\case
                Left err
                  | isDoesNotExistError err ->
                    do errprint (mappend "Oops! You don't appear to have a louse repository in " dir)
                       errprint "Hint: Try running `louse init`."
                       exitFailure
                  | isPermissionError err ->
                    do errprint (mappend "I got a permission error when trying to read the louse repo in "
                                         dir)
                       errprint "Do you have permission to read this directory?"
                       exitFailure
                  | isAlreadyInUseError err ->
                    do fail (mconcat ["Another process is using the louse repo in "
                                     ,dir
                                     ,". I don't know what to do about that, so I'm just going to quit."])
                  | otherwise -> ioError err
                Right l -> pure l)
     let bugs = louseBugs louse
         nTotalBugs = M.size bugs
         nOpenBugs =
           length (M.filter bugOpen bugs)
         ratioOf = (%)
         closureRateStr
           | nTotalBugs == 0 = mempty
           | otherwise =
             mconcat ["Closure rate: ",show (ratioOf nOpenBugs nTotalBugs),"%."]
     pure (unlines [mappend "Louse directory: " dir
                   ,mappend "Open bugs: " (show nOpenBugs)
                   ,closureRateStr])
