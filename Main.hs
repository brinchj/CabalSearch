module Main where

import Control.Monad

import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Posix.User (getUserEntryForName, getEffectiveUserName, UserEntry(..))
import Data.List

import CabalIndex

ensureIndex :: FilePath -> String -> Bool -> IO ()
ensureIndex dir name force =
  do -- Check directory
     dirExists <- doesDirectoryExist dir
     unless dirExists $
       do putStrLn $ "Creating: " ++ dir
          createDirectory dir
     -- Check db file
     dbExists <- doesFileExist name
     when (force || not dbExists) $
       do putStrLn $ "Building database: " ++ name
          buildDatabase dir name

main :: IO ()
main = do
  args    <- getArgs
  homeDir <- homeDirectory `fmap` (getUserEntryForName =<< getEffectiveUserName)
  let dbDir  = homeDir </> ".cabalsearch"
      dbPath = dbDir   </> "packages.sqlite3"
      arg    = head args
  -- show info
  when (length args /= 1 || "--help" `elem` args) $
    do name <- getProgName
       putStrLn $ "usage: " ++ name ++ " searchTerm"
       putStrLn $ "   or: " ++ name ++ " --rebuild"
       exitFailure
  -- work
  go (arg `isPrefixOf` "--rebuild") arg dbDir dbPath
  where
    go rebuild term dbDir dbPath =
      do -- rebuild index
        when rebuild $
          do ensureIndex dbDir dbPath True
             exitSuccess
        -- run query
        ensureIndex dbDir dbPath False
        res <- queryPackages (dbDir </> dbPath) term
        forM_ res $ \(name, meta) -> putStrLn $ concat
                                     ["* ", name, "\n", meta, "\n"]


