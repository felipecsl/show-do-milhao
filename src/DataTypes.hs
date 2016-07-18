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

type ProgramData = [QuestionGroup]
