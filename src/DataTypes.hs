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

type Rounds = [[Int]]

type ProgramData = [QuestionGroup]
