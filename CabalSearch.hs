module Main where

import Control.Monad

import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Posix.User (getUserEntryForName, getEffectiveUserName, UserEntry(..))
import Data.List

import CabalIndex

ensureIndex dir name =
  do dirExists <- doesDirectoryExist dir
     when (not dirExists) $
       do putStrLn $ "Creating: " ++ dir
          createDirectory dir
          ensureIndex dir name
     dbExists <- doesFileExist name
     when (not dbExists) $
       do putStrLn $ "Building database: " ++ name
          buildDatabase dir name

main = do
  args    <- getArgs
  homeDir <- homeDirectory `fmap` (getUserEntryForName =<< getEffectiveUserName)
  let dbDir  = homeDir </> ".cabalsearch"
      dbPath = dbDir   </> "packages.sqlite3"
      arg    = head args
  -- show info
  when (length args /= 1) $
    do name <- getProgName
       putStrLn $ "usage: " ++ name ++ " searchTerm"
       putStrLn $ "       " ++ name ++ " --rebuild"
       exitFailure
  -- work
  go (arg `isPrefixOf` "--rebuild") arg dbDir dbPath
  where
    go rebuild term dbDir dbPath =
      do -- rebuild index
        when (rebuild) $
          do removeFile $ dbPath
             ensureIndex dbDir dbPath
             exitSuccess
        -- run query
        ensureIndex dbDir dbPath
        res <- queryPackages (dbDir </> dbPath) term
        flip mapM_ res $ \(name, meta) -> do putStrLn $ concat $
                                               ["* ", name, "\n", meta, "\n"]


