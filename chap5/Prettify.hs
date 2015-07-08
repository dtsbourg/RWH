module Prettify where

import Numeric
import Data.Bits
import Data.Char (ord)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

string :: String -> Doc
string = quote . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double = text . show

line :: Doc
line = Line

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series o c prtr = enclose o c . fsep . punctuate (char ',') . map prtr

quote :: Doc -> Doc
quote = enclose '"' '"'

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

char :: Char -> Doc
char = Char

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> y

softLine :: Doc
softLine = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

oneChar :: Char -> Doc
oneChar c = case lookup c escapes of
                Just r -> text r
                Nothing | shouldEscape c -> hexEscape c
                        | otherwise    -> char c
                          where shouldEscape c = c < ' ' || c == '\x7f' || c > '\xff'

escapes :: [(Char, String)] -- a(ssociation)list
escapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
              where d = ord c

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) = case d of
                      Empty        -> transform ds
                      Char c       -> c : transform ds
                      Text s       -> s ++ transform ds
                      Line         -> '\n' : transform ds
                      a `Concat` b -> transform (a:b:ds)
                      _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty w x = best 0 [x]
             where best col (d:ds) =
                      case d of
                    Empty      -> best col ds
                    Char c     -> c : best (col+1) ds
                    Text s     -> s ++ best (col + length s) ds
                    Line       -> '\n' : best 0 ds
                    Concat a b -> best col (a:b:ds)
                    Union a b  -> nicest col (best col (a:ds)) (best col (b:ds))
                   best _ _ = ""

                   nicest col a b | (w - l) `fits` a = a
                                  | otherwise        = b
                                   where l = min w col


fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs


-- Exercices

-- Adds spaces to the Doc until we get to the desired number of lines
fill :: Int -> Doc -> Doc
fill = undefined

-- Indent parens brackets etc
indent :: Int -> Doc -> Doc
indent = undefined


