module Advent where

--- Minimum Stack Implementation

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x stack = x : stack

pop :: Stack a -> (a, Stack a)
pop [] = error "empty list"
pop (x : xs) = (x, xs)

peek :: Stack a -> a
peek [] = error "empty list"
peek (x : _) = x

safePop :: Stack a -> Maybe (a, Stack a)
safePop [] = Nothing
safePop (x : xs) = Just (x, xs)

safePeek :: Stack a -> Maybe a
safePeek [] = Nothing
safePeek (x : _) = Just x

-- Minimum Queue Implmentation

type Queue a = [a]

queueAdd :: a -> Queue a -> Queue a
queueAdd a qs = qs ++ [a]

queueRemove :: Queue a -> (a, Queue a)
queueRemove [] = error "empty list"
queueRemove (q : qs) = (q, qs)

queuePeek :: Queue a -> a
queuePeek [] = error "empty list"
queuePeek (q : _) = q

safeQueueRemove :: Queue a -> Maybe (a, Queue a)
safeQueueRemove [] = Nothing
safeQueueRemove (q : qs) = Just (q, qs)

safeQueuePeek :: Queue a -> Maybe a
safeQueuePeek [] = Nothing
safeQueuePeek (q : _) = Just q
