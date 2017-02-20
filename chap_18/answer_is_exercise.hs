module AnswerIsExercise where

import Control.Monad (join)
import Control.Applicative

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a