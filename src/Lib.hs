module Lib
    ( libMain
    ) where

import           Data.ByteString       (ByteString (..), concat, hGetContents,
                                        intercalate, isPrefixOf)
import           Data.ByteString.Char8 (lines, pack, singleton, unpack)
import           Data.Char             (ord)
import           Data.List             (elemIndex)
import           Data.List.Split       (splitWhen)
import           Data.String.Utils     (startswith)
import qualified Data.Text             as T
import           Data.Text.ICU.Convert (open, toUnicode)
import qualified Data.Text.IO          as T
import           Data.Word             (Word8 (..))
import           Prelude               hiding (concat, lines)
import           System.IO             (IOMode (..), openFile)


data Question = Question {
      statement :: ByteString
    , options   :: [ByteString]
    , answer    :: Int
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
        correct_answer = answer q
        indexedOpts = mapInd (\x i -> concat ([singleton i] ++ [pack ") "] ++ [x])) opts
    byteStringToString stmt >>= putStrLn
    byteStringToString (intercalate (pack "\n") indexedOpts) >>= putStrLn
    putStrLn "Resposta? "
    answr <- getChar
    let intAnswr = elemIndex answr ['a'..]
    return (intAnswr == Just correct_answer)

listToQuestion :: [ByteString] -> Question
listToQuestion (x:xs) = Question x xs 1

parseSections :: ByteString -> [[Question]]
parseSections bs =
    let nonEmptyString = filter (/= pack "")
        isSectionMarker = isPrefixOf $ pack "###"
        questionGroup = group 5
        allLines = lines bs
        sections = filter (not . null) $ splitWhen isSectionMarker allLines
        groupedQuestionListBySection = map (questionGroup . nonEmptyString) sections
        answers = last groupedQuestionListBySection
    in map (map listToQuestion) (init groupedQuestionListBySection)

libMain :: IO ()
libMain = do
    h <- openFile "questions.txt" ReadMode
    bs <- hGetContents h
    let sections = parseSections bs
    let easy = head sections
    let medium = sections !! 1
    let hard = sections !! 2
    presentQuestion (easy !! 30) >>= print
    return ()
    -- putStrLn $ byteStringToString conv (intercalate (pack "\n") (easy !! 0))
    -- let questions = [ Question {
    --       statement = "Em que estado brasileiro nasceu a apresentadora Xuxa?"
    --     , options = [ "Rio de Janeiro", "Rio Grande do Sul", "Santa Catarina", "Goiás" ]
    --     , answer = 1
    --   }]
    -- result <- presentQuestion (questions !! 0)
    -- putStrLn (if result)
    -- return ()
