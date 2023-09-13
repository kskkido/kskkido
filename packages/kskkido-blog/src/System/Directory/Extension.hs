module System.Directory.Extension where

import RIO
import qualified RIO.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified Control.Monad.Extra as Extra

writeFile :: IO.FilePath -> String -> IO.IO ()
writeFile path file = do
  let directory = FilePath.takeDirectory path
  IO.print directory
  Directory.createDirectoryIfMissing True directory
  IO.writeFile path file

copyDirectory :: String -> String -> IO.IO [IO.FilePath]
copyDirectory source destination = do
  Directory.createDirectoryIfMissing True destination
  paths <- Directory.listDirectory source
  flip foldMap paths $ \path -> do
    let sourcePath = source FilePath.</> path
        destinationPath =  destination FilePath.</> path
    Extra.ifM
      ( Directory.doesDirectoryExist sourcePath )
      ( copyDirectory sourcePath destinationPath )
      ( Directory.copyFile sourcePath destinationPath $> [destinationPath] )

removeDirectory :: IO.FilePath -> IO.IO ()
removeDirectory filePath = do
  Extra.whenM (Directory.doesDirectoryExist filePath) do
    Directory.removeDirectoryRecursive filePath

listFilePaths :: String -> IO.IO [IO.FilePath]
listFilePaths source = do
  paths <- Directory.listDirectory source
  flip foldMap paths $ \path -> do
    let sourcePath = source FilePath.</> path
    Extra.ifM
      ( Directory.doesDirectoryExist sourcePath )
      ( listFilePaths sourcePath )
      ( pure [sourcePath] )

listRelativeFilePaths :: String -> IO.IO [IO.FilePath]
listRelativeFilePaths source = do
  paths <- Directory.listDirectory source
  flip foldMap paths $ \path -> do
    let sourcePath = source FilePath.</> path
    Extra.ifM
      ( Directory.doesDirectoryExist sourcePath )
      ( fmap (path FilePath.</>) <$> listRelativeFilePaths sourcePath )
      ( pure [path] )
