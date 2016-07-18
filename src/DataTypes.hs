module DataTypes where

import           Data.ByteString (ByteString (..))

data Question = Question {
    statement :: ByteString
  , options   :: [ByteString]
  , answer    :: Int
}

data QuestionGroup = QuestionGroup {
    difficulty :: ByteString
  , questions  :: [Question]
}

-- Cash prizes for correct answer, stop playing and wrong answers, respectively
data Prizes = Prizes {
    correct :: [[Int]]
  , stop    :: [[Int]]
  , wrong   :: [[Int]]
}

type ProgramData = [QuestionGroup]
