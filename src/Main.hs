module Main where

import           PackageInfo    (packageInfo)
import           Template       (createTemplate)

import           System.Exit    (ExitCode (..))
import           System.Process

checkProgram :: FilePath -> IO ()
checkProgram program = do
  (_, _, _, ph) <- createProcess (proc program ["--version"])
                   { std_out = CreatePipe, std_err = CreatePipe }
  exitcode <- waitForProcess ph
  case exitcode of
    ExitFailure r -> error $ program ++ " seems to be not installed (" ++
                     show r ++ ")."
    ExitSuccess   -> putStrLn $ "-- Found " ++ program ++ "."

main :: IO ()
main = do
  mapM_ checkProgram ["autoreconf", "autoconf", "automake"]
  pkg <- packageInfo
  createTemplate pkg
