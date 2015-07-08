module Parse where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char
import Control.Applicative ((<$>))

data ParseState = ParseState {
   string :: L.ByteString
 , offset :: Int64
}

-- wrapper to hide parser impl
newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

instance Functor Parse where
  fmap f parser = parser ==> \result ->identity (f result)

{--}
parse :: Parse a ->L.ByteString ->Either String a
parse parser init = case runParse parser (ParseState init 0) of
                         Left err      ->Left err
                         Right (res,_) ->Right res

{-Parsers-}
identity :: a ->Parse a
identity a = Parse (\s -> Right (a,s))

parseByte :: Parse Word8
parseByte = getState ==> \initState ->
            case L.uncons (string initState) of
                 Nothing ->bail "No more input"
                 Just (byte, remainder) ->
                   putState newState ==> \_ ->
                     identity byte
                  where newState = initState { string = remainder,
                                               offset = newOffset }
                        newOffset = offset initState + 1


parseChar :: Parse Char
parseChar = fmap w2c parseByte

walkByte :: Parse (Maybe Word8)
walkByte = fmap (fmap fst . L.uncons . string) getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> walkByte

{--}
modifyOffset :: ParseState ->Int64 ->ParseState
modifyOffset ps off = ps { offset = off }

getState :: Parse ParseState
getState = Parse (\s -> Right (s,s))

putState :: ParseState ->Parse ()
putState s = Parse (\_ ->Right ((),s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a ->(a ->Parse b) ->Parse b
(Parse fs) ==> fa = Parse newFs
  where newFs s = case fs s of
          Left err      -> Left err
          Right (a, s') -> runParse (fa a) s'

w2c :: Word8 ->Char
w2c = chr . fromIntegral

