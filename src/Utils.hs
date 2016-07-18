module Utils where

import           Data.ByteString       (ByteString (..))
import           Data.ByteString.Char8 (unpack)
import           Data.List.Split       (keepDelimsL, split, whenElt)
import qualified Data.Text             as T
import           Data.Text.ICU.Convert (open, toUnicode)
import qualified Data.Text.IO          as T

mapIndChar :: (a -> Char -> b) -> [a] -> [b]
mapIndChar f l = zipWith f l ['a'..]

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative n"

byteStringToString :: ByteString -> IO String
byteStringToString s = do
    conv <- open "utf-8" Nothing
    return (T.unpack $ toUnicode conv s)

byteStringToInt :: ByteString -> Int
byteStringToInt s = read (unpack s) :: Int

doWhileM :: (a -> IO Bool) -> [a] -> IO Bool
doWhileM _ [] = return False
doWhileM m (x:xs) = do
    res <- m x
    if res
      then doWhileM m xs
      else return False

-- Split an array using the function for selecting the delimiter. The resulting array includes
-- the delimiter as the first item in the array. This is important because we need the headers for
-- each question group (Facil, Medio and Dificil)
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = split . keepDelimsL . whenElt
