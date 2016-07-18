module Lib
    ( libMain
    ) where

import           Control.Monad         (when)
import           Data.ByteString       (ByteString (..), concat, hGetContents,
                                        intercalate, isPrefixOf)
import           Data.ByteString.Char8 (lines, pack, singleton, unpack)
import           Data.Char             (digitToInt, ord)
import           Data.List             (elemIndex, zip4)
import           Data.Maybe            (fromMaybe)
import           Data.String.Utils     (startswith)
import           Data.Word             (Word8 (..))
import           DataTypes
import           Prelude               hiding (lines)
import           System.IO             (BufferMode (..), IOMode (..),
                                        hSetBuffering, openFile, stdin)
import           System.Random.Shuffle (shuffleM)
import           Utils

-- Takes a question and how much it's worth
presentQuestion :: (Question, Int, Int, Int) -> IO Bool
presentQuestion (q, correct, stop, wrong) = do
    let stmt = statement q
        opts = options q
        -- The answers are 1-indexed (instead of 0), so we need to subtract 1 to offset it
        ans = answer q
    byteStringToString stmt >>= putStrLn
    byteStringToString (intercalate (pack "\n") opts) >>= putStrLn
    putStrLn $ "Errar: R$ " ++ show wrong ++ ", Parar: R$ " ++ show stop ++ ", Acertar: R$ " ++ show correct
    putStrLn "Resposta? "
    answr <- getChar
    let didAnswerCorrectly = digitToInt answr == ans
    if didAnswerCorrectly
      then putStrLn "\nCerta resposta!\n"
      else putStrLn $ "\nResposta errada. A resposta certa é " ++ show ans
    return didAnswerCorrectly

-- Converts an array of strings (with 5 elements) into a Question object
listToQuestion :: (Int, [ByteString]) -> Question
listToQuestion (ans, x:xs) = Question x xs ans

-- Builds a QuestionGroup based on a nested array of ByteStrings, where the head of each group
-- is the header and the tail are the groups of questions (with 5 elements each)
listToQuestionGroups :: [Int] -> [[ByteString]] -> QuestionGroup
listToQuestionGroups a (x:xs) = QuestionGroup (head x) (zipWith (curry listToQuestion) a xs)

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
    in parseGroupsWithAnswers answers (init xs)

-- Takes a ByteString with the input file contents and parse it into a ProgramData object
parseSections :: ByteString -> IO ProgramData
parseSections bs =
    let nonEmptyString = filter (/= pack "")
        isSectionMarker = isPrefixOf (pack "###")
        sections = filter (not . null) (splitWhen isSectionMarker (lines bs))
        groupedQuestionListBySection = map (questionGroup . nonEmptyString) sections
    in return (buildProgramData groupedQuestionListBySection)

printQuestionGroup :: [Int] -> [Int] -> [Int] -> QuestionGroup -> IO Bool
printQuestionGroup cs ss ws g = do
    qs <- shuffleM $ questions g
    byteStringToString (difficulty g) >>= putStrLn
    doWhileM presentQuestion (zip4 qs cs ss ws)

printQuestionGroups :: Prizes -> ProgramData -> IO ()
printQuestionGroups p (x:xs) = do
    let (c:cs) = correct p
        (s:ss) = stop p
        (w:ws) = wrong p
    res <- printQuestionGroup c s w x
    when res $ printQuestionGroups (Prizes cs ss ws) xs

libMain :: IO ()
libMain = do
    -- O programa consistia em três rodadas e uma pergunta final: a primeira continha 5 perguntas,
    -- cada uma valendo mil reais cumulativos. A segunda, de 5 perguntas valendo R$ 10 mil
    -- cumulativos cada. A terceira, de 5 perguntas de R$100 mil reais cumulativos cada.
    -- A última pergunta valia R$ 1 milhão.
    let winRange = [1..5]
        stopRange = [5, 10, 20, 30, 40]
        errorRange = [2, 5, 10, 15, 20]
        correctPrizes = [map (*1000) winRange] ++ [map (*10000) winRange] ++ [map (*100000) winRange] ++ [[1000000]]
        stopPrizes = [map (*1000) [0..5]] ++ [map (*1000) stopRange] ++ [map (*10000) stopRange] ++ [[500000]]
        errorPrizes = [map (*500) [0..4]] ++ [map (*1000) errorRange] ++ [map (*10000) errorRange] ++ [[0]]
        prizes = Prizes correctPrizes stopPrizes errorPrizes
    -- We need to disable stdin buffering otherwise getChar won't work well
    hSetBuffering stdin NoBuffering
    openFile "questions.txt" ReadMode >>= hGetContents >>= parseSections >>= printQuestionGroups prizes
