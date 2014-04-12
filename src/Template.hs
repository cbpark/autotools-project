{-# LANGUAGE RecordWildCards #-}

module Template (createTemplate) where

import           License          (createLicense)
import           Types            (Package (..))

import           Data.Maybe       (fromMaybe)
import           System.Directory (Permissions (..), createDirectory,
                                   getPermissions, setPermissions)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.IO        (IOMode (..), hGetLine, hPutStr, withFile)
import           System.Process   (StdStream (..), createProcess, proc, std_out,
                                   waitForProcess)

createTemplate :: Package -> IO ()
createTemplate pkg = do
  prepareDirectories
  writeFiles pkg
  (_, _, _, ph) <- createProcess (proc "autoreconf" ["-ivf"])
  exitcode <- waitForProcess ph
  case exitcode of
    ExitFailure r -> error $ show r
    ExitSuccess   -> putStrLn "-- Done. May the source be with you!"

prepareDirectories :: IO ()
prepareDirectories = do
  putStrLn "Creating diectories..."
  mapM_ createDirectory ["config", "m4", "src"]

writeFiles :: Package -> IO ()
writeFiles pkg@Package{ .. } = do
  -- Write Makefile.am in the top-level directory
  putStrLn "Generating Makefile.am..."
  createFile "Makefile.am" $ unlines [ "ACLOCAL_AMFLAGS = -I m4"
                                     , "SUBDIRS = src"
                                     , "EXTRA_DIST = autogen.sh"
                                     ]

  -- Write Makefile.am in the src directory
  putStrLn "Generating src/Makefile.am..."
  createFile ("src" </> "Makefile.am") $ unlines
                               [ "AM_CFLAGS = -Wall"
                               , ""
                               , "bin_PROGRAMS = hello"
                               , ""
                               , "hello_SOURCES = hello.c"
                               , "hello_LDADD ="
                               , ""
                               , "pkginclude_HEADERS ="
                               ]

  -- Write a example code
  putStrLn "Generating a sameple code (src/hello.c)..."
  createFile ("src" </> "hello.c") $ unlines
                               [ "#include <stdio.h>"
                               , ""
                               , "int main()"
                               , "{"
                               , "    printf(\"Hello, world!\\n\");"
                               , ""
                               , "    return 0;"
                               , "}"
                               ]

  -- Write configure.ac
  configStr <- configAC pkg
  putStrLn "Generating configure.ac..."
  createFile "configure.ac" configStr

  -- Write README
  putStrLn "Generating README..."
  createFile "README" $ unlines [ pkgName
                                , replicate (length pkgName) '='
                                , ""
                                , fromMaybe "" description
                                ]

  -- Write LICENSE
  putStrLn "Generating LICENSE..."
  licenseStr <- createLicense pkg
  case licenseStr of
    Nothing -> putStrLn " ... Oops! You have chosen no License."
    Just ls -> createFile "LICENSE" ls

  -- Write .gitignore
  putStrLn "Generating .gitignore..."
  createFile ".gitgnore" $ unlines
                 [ "*.o", "*.lo", "*.a", "*.la", "*/.deps", "*/.libs"
                 , "*.gz", "*.orig", "*~", "autom4te.cache", "config.log"
                 , "config.status", "config/config.h", "config/stamp-h1"
                 , "libtool", "Makefile"
                 ]

  -- Write autogen.sh
  putStrLn "Generating autogen.sh..."
  createFile "autogen.sh" $ unlines [ "autoreconf -ivf -I config -I m4" ]
  -- Make autogen.sh be executable
  p <- getPermissions "autogen.sh"
  setPermissions "autogen.sh" (p {executable = True})

createFile :: FilePath -> String -> IO ()
createFile file contents = withFile file WriteMode $ \handle ->
                           hPutStr handle contents

configAC :: Package -> IO String
configAC Package { .. } = do
    acVersion <- getACVersion
    return $ unlines
               [ "#                                               -*- Autoconf -*-"
               , "# Process this file with autoconf to produce a configure script."
               , ""
               , "AC_PREREQ([" ++ acVersion ++ "])"
               , "AC_INIT([" ++ pkgName ++ "], [" ++ version ++ "]" ++
                 case email of Just m  -> ", [" ++ m ++ "])"
                               Nothing -> ")"
               , "AC_CONFIG_SRCDIR([src/hello.c])"
               , "AC_CONFIG_AUX_DIR([config])"
               , "AC_CONFIG_HEADERS([config/config.h])"
               , "AC_CONFIG_MACRO_DIR([m4])"
               , "# m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])"
               , ""
               , "# Checks for programs."
               , "AM_INIT_AUTOMAKE([-Wall -Werror foreign])"
               , "AC_PROG_CC"
               , "AC_PROG_CXX"
               , "AM_PROG_AR"
               , ""
               , "# Checks for libraries."
               , ""
               , "# Checks for header files."
               , "AC_HEADER_STDC"
               , ""
               , "# Checks for typedefs, structures, and compiler characteristics."
               , "AC_TYPE_SIZE_T"
               , ""
               , "# Checks for library functions."
               , ""
               , "AC_CONFIG_FILES([Makefile src/Makefile])"
               , "AC_OUTPUT"
               ]

getACVersion :: IO String
getACVersion = do
  (_, Just hout, _, _) <- createProcess
                          (proc "autoreconf" ["--version", "|", "head", "-n", "1"])
                          { std_out = CreatePipe }
  verstr <- hGetLine hout
  return $ (last . words) verstr
