{- GlobRegex.hs
 - Taking a glob (eg "file??.*") and turn it into Regex
 -}

module GlobRegex where

import Text.Regex.Posix ((=~))
import Data.Char
import System.FilePath

data Case = Sensitive | Insensitive deriving (Eq)

{-API-}
-- Returns a bool indicating if File matches glob
matchesGlob :: FilePath -> String -> Bool
matchesGlob = matchesGlobCase caseSensitive

matchesGlobCase :: Case -> FilePath -> String -> Bool
matchesGlobCase c f p= f =~ globToRegex p c

-- Transforms glob -> regex
globToRegex :: String -> Case -> String
globToRegex "" _             = ""
globToRegex ('*':xs) b       = ".*" ++ globToRegex xs b
globToRegex ('?':xs) b       = '.' : globToRegex xs b
globToRegex ('[':'!':x:xs) b = "[^" ++ x : charClass xs b
globToRegex ('[':x:xs) b     = '[' : x : charClass xs b
globToRegex ('[':_) _        = error "Undetermined character class"
globToRegex (x:xs) b         = escape x b ++ globToRegex xs b


{-HELPERS-}
-- Checks if character class terminates correctly and passes trough the text
charClass :: String -> Case -> String
charClass (']':xs) b = ']' : globToRegex xs b
charClass (c:cs) b   = c : charClass cs b
charClass [] _       = error "Undetermined character class"

-- Escape characters
escape :: Char -> Case -> String
escape c b | c `elem` regexChars           = '\\' : [c]
           | isAlpha c && b == Insensitive  = '[' : [toUpper c] ++ "|" ++ [toLower c] ++ "]"
           | otherwise                     = [c]
             where regexChars = "\\+()^$.{}]|"

-- CS <-> UNIX || CI <-> Windows
caseSensitive :: Case
caseSensitive | isPathSeparator '\\' = Insensitive
              | otherwise = Sensitive
