module Lib
    ( libMain
    ) where

import           Control.Monad         (when)
import           Data.ByteString       (ByteString (..), concat, hGetContents,
                                        intercalate, isPrefixOf)
import           Data.ByteString.Char8 (lines, pack, singleton, unpack)
import           Data.Char             (digitToInt, ord)
import           Data.List             (elemIndex)
import           Data.Maybe            (fromMaybe)
import           Data.String.Utils     (startswith)
import           Data.Word             (Word8 (..))
import           DataTypes
import           Prelude               hiding (lines)
import           System.IO             (BufferMode (..), IOMode (..),
                                        hSetBuffering, openFile, stdin)
import           Utils

questionStatement :: ByteString -> Int -> IO String
questionStatement stmt amt = do
    let fullStmt = pack $ "Pergunta valendo R$ " ++ show amt ++ ": "
    byteStringToString $ Data.ByteString.concat (fullStmt : [stmt])

-- Takes a question and how much it's worth
presentQuestion :: (Question, Int) -> IO Bool
presentQuestion (q, amt) = do
    let stmt = statement q
        opts = options q
        -- The answers are 1-indexed (instead of 0), so we need to subtract 1 to offset it
        ans = answer q
    questionStatement stmt amt >>= putStrLn
    byteStringToString (intercalate (pack "\n") opts) >>= putStrLn
    putStrLn "Resposta? "
    answr <- getChar
    let didAnswerCorrectly = digitToInt answr == ans
    if didAnswerCorrectly
      then putStrLn "\nCerta resposta!\n"
      else putStrLn ("\nResposta errada. A resposta certa é " ++ show ans)
    return didAnswerCorrectly

-- Converts an array of strings (with 5 elements) into a Question object
listToQuestion :: (Int, [ByteString]) -> Question
listToQuestion a =
    let ans = fst a
        (x:xs) = snd a
    in Question x xs ans

-- Builds a QuestionGroup based on a nested array of ByteStrings, where the head of each group
-- is the header and the tail are the groups of questions (with 5 elements each)
listToQuestionGroups :: [Int] -> [[ByteString]] -> QuestionGroup
listToQuestionGroups a xs = QuestionGroup (head $ head xs) (zipWith (curry listToQuestion) a (tail xs))

-- This groups questions in lists of 5 elements, where:
-- 1st item is the question statement
-- items 2, 3, 4 and 5 are the question possible answers (a, b, c or d)
-- The list head is the title of the question group (either ### Facil, ### Medio or ### Dificil),
-- so that is not included in the grouping
questionGroup :: [ByteString] -> [[ByteString]]
questionGroup (x:xs) = [x] : group 5 xs

-- Takes an array of answers and an array of sections with question groups and parses them into
-- an array of question groups
parseGroupsWithAnswers :: [Int] -> [[[ByteString]]] -> [QuestionGroup]
parseGroupsWithAnswers a (x:xs) =
    -- Need to subtract one for the section header
    let totalQuestions = length x - 1
        thisGroup = listToQuestionGroups (take totalQuestions a) x
        remainder = parseGroupsWithAnswers (drop totalQuestions a) xs
    in thisGroup : remainder

-- This array should have 4 items
-- 1st item - [[ByteString]] of Easy questions
-- 2nd item - [[ByteString]] of Medium questions
-- 3rd item - [[ByteString]] of Hard questions
-- 4rd item - [[ByteString]] of answers - This needs to be flattened into a [ByteString] with concat
buildProgramData :: [[[ByteString]]] -> ProgramData
buildProgramData xs =
    let answers = map byteStringToInt (tail $ Prelude.concat (last xs))
        groups = parseGroupsWithAnswers answers (init xs)
    in groups

-- Takes a ByteString with the input file contents and parse it into a ProgramData object
parseSections :: ByteString -> IO ProgramData
parseSections bs =
    let nonEmptyString = filter (/= pack "")
        isSectionMarker = isPrefixOf (pack "###")
        allLines = lines bs
        sections = filter (not . null) (splitWhen isSectionMarker allLines)
        groupedQuestionListBySection = map (questionGroup . nonEmptyString) sections
    in return (buildProgramData groupedQuestionListBySection)

printQuestionGroup :: [Int] -> QuestionGroup -> IO Bool
printQuestionGroup rs g = do
    let qs = questions g
    byteStringToString (difficulty g) >>= putStrLn
    doWhileM presentQuestion (zip qs rs)

printQuestionGroups :: Rounds -> ProgramData -> IO ()
printQuestionGroups (r:rs) (x:xs) = do
    res <- printQuestionGroup r x
    when res $ printQuestionGroups rs xs

libMain :: IO ()
libMain = do
    -- O programa consistia em três rodadas e uma pergunta final: a primeira continha 5 perguntas,
    -- cada uma valendo mil reais cumulativos. A segunda, de 5 perguntas valendo R$ 10 mil
    -- cumulativos cada. A terceira, de 5 perguntas de R$100 mil reais cumulativos cada.
    -- A última pergunta valia R$ 1 milhão.
    let rounds = [map (*1000) [1..5]] ++ [map (*1000) [10..50]] ++ [map (*1000) [100..500] ++ [1000000]]
    -- We need to disable stdin buffering otherwise getChar won't work well
    hSetBuffering stdin NoBuffering
    openFile "questions.txt" ReadMode >>= hGetContents >>= parseSections >>= printQuestionGroups rounds
