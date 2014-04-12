module Types
    (
      Package (..)
    , License (..)
    ) where

data Package = Package {
      pkgName     :: String
    , version     :: String
    , license     :: License
    , authorName  :: Maybe String
    , email       :: Maybe String
    , description :: Maybe String
    } deriving Show

data License = NoLicense
             | GPL2
             | GPL3
             | LGPL2
             | LGPL3
             | AGPL3
             | BSD3
             | MIT
             | Apache
             | WTFPL
               deriving (Show, Eq, Enum, Bounded)
