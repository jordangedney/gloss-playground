-- https://rafal.io/posts/haskell-queues.html

module Queue
  ( Queue
  , emptyQueue
  , push
  , pop
  , peek
  , pushQueue
  , popQueue
  ) where

import Control.Monad.Trans.State (State, state)

data Queue a
  = Queue
  { inbox':: [a]
  , outbox':: [a]
  } deriving (Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

push :: a -> Queue a -> Queue a
push elementToAdd (Queue inbox outbox)
  = Queue (elementToAdd : inbox) outbox

pop ::  Queue a -> (Maybe a, Queue a)
pop queue =
  case top of
    Nothing -> (Nothing, emptyQueue)
    Just element -> (Just element, poppedQueue)
    where (top, queue') = peek queue
          poppedQueue = Queue (inbox' queue') (tail $ outbox' queue')

peek :: Queue a -> (Maybe a, Queue a)
peek (Queue [] []) = (Nothing, emptyQueue)
peek (Queue inbox[]) = peek $ Queue [] $ reverse inbox
peek queue@(Queue _ (x:_)) = (Just x, queue)

type QueueState a = State (Queue a)

pushQueue :: a -> QueueState a ()
pushQueue element =
  state $
  \queue -> ((), push element queue)

popQueue :: QueueState a (Maybe a)
popQueue =
  state $
  \queue -> pop queue
