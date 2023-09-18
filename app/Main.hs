module Main ( main ) where

import           Control.Monad

import           Options.Applicative

import           System.Directory
import           System.FilePath.Posix
import           System.IO             ( hPutStrLn
                                       , stderr )

data Options =
    Options { optVerbose :: Bool, optMove :: Bool, optFiles :: [FilePath] }

parseOptions :: Parser Options
parseOptions = Options
    <$> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
    <*> switch (long "move" <> short 'm' <> help "Move files instead of copying")
    <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = do
  options <- execParser (info (parseOptions <**> helper)
                              (progDesc "Backup files" <> fullDesc))
  let verbose = optVerbose options
      move    = optMove options
      files   = optFiles options
  forM_ files $ \file -> do
    let backupFile = addExtension file "bak"
    fileExists <- doesFileExist file
    if fileExists
        then do
          if move
              then do
                renameFile file backupFile
                when verbose $ putStrLn
                    $ "Moved " ++ file ++ " to " ++ backupFile
              else do
                copyFile file backupFile
                when verbose $ putStrLn
                    $ "Backed up " ++ file ++ " to " ++ backupFile
        else hPutStrLn stderr $ "File " ++ file ++ " does not exist."
  when verbose $ putStrLn "Backup complete."
