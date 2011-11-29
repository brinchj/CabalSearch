module CabalIndex
       ( buildDatabase
       , queryPackages)
       where

import System.Unix.Directory (withTemporaryDirectory)
import System.Directory      (renameFile)
import System.FilePath       ((</>))
import System.Process        (readProcess)

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.List as L


{-| Insert packages into table -}
populate conn ps =
  do stmt <- prepare conn "INSERT INTO packages VALUES (?, ?)"
     executeMany stmt $ map (\(n,m) -> [toSql n, toSql m]) ps

{-| Create table for packages -}
createTable conn =
  run conn "CREATE TABLE packages (name VARCHAR(16) PRIMARY KEY, meta VARCHAR(128))" []

{-| Create and populate a new database, replacing the old -}
buildDatabase dbDir dbName =
  do -- place tmp file in same dir as db to allow atomic move
     let tmpDb = dbDir </> dbName ++ "-partial"
     -- build database in temporary path
     conn <- connectSqlite3 tmpDb
     createTable conn
     populate conn =<< parseCabal
     -- commit and disconnect
     commit conn
     disconnect conn
     -- move new database over old one
     renameFile tmpDb $ dbDir </> dbName

{-| Query a database, using the output from Cabal -}
queryPackages db term =
  do conn <- connectSqlite3 db
     rows <- quickQuery conn "SELECT * FROM packages WHERE name LIKE ?" [toSql $ '%':term++"%"]
     return $ map (\[n,m] -> (fromSql n :: String, fromSql m :: String)) rows


{-| List packages using Cabal and parse the result to (name, meta) -}
parseCabal = go `fmap` lines `fmap` readProcess "cabal" ["list"] []
  where
    go [] = []
    go (('*':' ':name):rest) =
      let (meta, rest') = gm [] rest
      in (name, L.intercalate "\n" meta) : go rest'

    gm m [] = (m, [])
    gm m (rest@(('*':_):_)) = (m, rest)
    gm m (x:xs)
      | L.null x  = gm m xs
      | otherwise = gm (m ++ [x]) xs


