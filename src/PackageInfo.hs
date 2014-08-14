module PackageInfo (packageInfo) where

import           Types                     (License (..), Package (..))

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Char                 (isDigit)
import           System.Directory          (getCurrentDirectory)
import           System.FilePath           (takeBaseName)
import           System.IO                 (hFlush, stdout)

packageInfo :: IO Package
packageInfo = do
  pkgName'     <- getPackageName
  version'     <- getVersion
  license'     <- getLicense
  authorName'  <- runMaybeT getAuthorName
  email'       <- runMaybeT getEmail
  description' <- runMaybeT getDescription
  return Package { pkgName     = pkgName'
                 , version     = version'
                 , license     = license'
                 , authorName  = authorName'
                 , email       = email'
                 , description = description'
                 }

getPackageName :: IO String
getPackageName = do
  curDir <- getCurrentDirectory
  let guess = takeBaseName curDir
  putStr $ "Package name? [default: " ++ guess ++ "] "
  input <- userInput (Just guess)
  case input of
    Just name -> return name
    _         -> error $ show input ++ " is not valid."

getVersion :: IO String
getVersion = do
  let guess = "0.1.0.0"
  putStr $ "Package version? [default: " ++ guess ++ "] "
  input <- userInput (Just guess)
  case input of
    Just ver -> return ver
    _        -> error $ show input ++ " is not valid."

getLicense :: IO License
getLicense = do
  putStrLn "Please choose a license:"
  mapM_ (putStrLn . printLicense) [NoLicense ..]
  getLicense' NoLicense
    where printLicense =
              (++) <$> (("  "++) . (++") ") . show . fromEnum) <*> show

          getLicense' dl = do
               putStr $ "Your choice [defualt: " ++ show dl ++ "] "
               hFlush stdout
               str <- getLine
               if null str
               then return dl
               else case safeReadLicenseNum str of
                      Just n -> return $ toEnum n
                      _      -> do putStrLn $ str ++ " is not valid."
                                   getLicense' dl

getAuthorName :: MaybeT IO String
getAuthorName = getInfoSimple "Author name? "

getEmail :: MaybeT IO String
getEmail = getInfoSimple "Maintainer email? "

getDescription :: MaybeT IO String
getDescription = getInfoSimple "Project description? "

getInfoSimple :: String -> MaybeT IO String
getInfoSimple question = do liftIO $ putStr question
                            (MaybeT . userInput) Nothing

userInput :: Maybe String -> IO (Maybe String)
userInput guess = do
  hFlush stdout
  str <- getLine
  return $ if null str then guess else Just str

safeReadLicenseNum :: String -> Maybe Int
safeReadLicenseNum s
    | all isDigit s = let num = read s :: Int
                          maxNum = fromEnum (maxBound :: License)
                      in if num > maxNum then Nothing else Just num
    | otherwise     = Nothing
