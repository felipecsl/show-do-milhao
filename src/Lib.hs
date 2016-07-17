module Lib
    ( libMain
    ) where

import           Control.Monad         (when)
import           Data.ByteString       (ByteString (..), concat, hGetContents,
                                        intercalate, isPrefixOf)
import           Data.ByteString.Char8 (lines, pack, singleton, unpack)
import           Data.Char             (digitToInt, ord)
import           Data.List             (elemIndex)
import           Data.List.Split       (splitWhen)
import           Data.Maybe            (fromMaybe)
import           Data.String.Utils     (startswith)
import qualified Data.Text             as T
import           Data.Text.ICU.Convert (open, toUnicode)
import qualified Data.Text.IO          as T
import           Data.Word             (Word8 (..))
import           Prelude               hiding (concat, lines)
import           System.IO             (BufferMode (..), IOMode (..),
                                        hSetBuffering, openFile, stdin)

data Question = Question {
      statement :: ByteString
    , options   :: [ByteString]
    , answer    :: Int
}

data QuestionGroup = QuestionGroup {
    difficulty  :: ByteString
    , questions :: [Question]
}

mapInd f l = zipWith f l ['a'..]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative n"

byteStringToString s = do
    conv <- open "utf-8" Nothing
    return (T.unpack $ toUnicode conv s)

presentQuestion :: Question -> IO Bool
presentQuestion q = do
    let stmt = statement q
        opts = options q
        correctAnswer = answer q
        indexedOpts = mapInd (\x i -> concat ([singleton i] ++ [pack ") "] ++ [x])) opts
    byteStringToString stmt >>= putStrLn
    byteStringToString (intercalate (pack "\n") indexedOpts) >>= putStrLn
    putStrLn "Resposta? "
    answr <- getChar
    let maybeAnswer = elemIndex answr ['a'..]
        didAnswerCorrectly = maybeAnswer == Just correctAnswer
    if didAnswerCorrectly
      then putStrLn "\nCerta resposta!\n"
      else putStrLn ("Resposta errada. Sua resposta foi " ++ [answr])
    return didAnswerCorrectly

listToQuestion :: [ByteString] -> Question
listToQuestion (x:xs) = Question x xs 1

listToQuestionGroups :: String -> [Question] -> QuestionGroup
listToQuestionGroups d = QuestionGroup (pack d)

parseSections :: IO ByteString -> IO [QuestionGroup]
parseSections bs =
    let nonEmptyString = filter (/= pack "")
        isSectionMarker = isPrefixOf (pack "###")
        questionGroup = group 5
        allLines = fmap lines bs
        sections = fmap (filter (not . null) . splitWhen isSectionMarker) allLines
        groupedQuestionListBySection = fmap (map (questionGroup . nonEmptyString)) sections
        l = listToQuestionGroups "easy" . map listToQuestion
        -- answers = last groupedQuestionListBySection
    in fmap (map l . init) groupedQuestionListBySection

doWhileM :: (a -> IO Bool) -> [a] -> IO ()
doWhileM _ [] = return ()
doWhileM m (x:xs) = do
    res <- m x
    when res $ doWhileM m xs

libMain :: IO ()
libMain = do
    hSetBuffering stdin NoBuffering
    let file = openFile "questions.txt" ReadMode >>= hGetContents
    sections <- parseSections file
    let easy = head sections
    let medium = sections !! 1
    let hard = sections !! 2
    doWhileM presentQuestion (questions easy)
    return ()
