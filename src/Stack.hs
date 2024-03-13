-- {-# LANGUAGE DeriveFoldable #-}
-- module Stack where

-- data Stack a = Empty | Push a Int (Stack a) deriving Foldable

-- push :: a -> Stack a -> Stack a
-- push a s@(Push _ n _) = Push a (n+1) s
-- push a Empty = Push a 1 Empty

-- -- modify ::a -> Stack a -> Stack a
-- -- modify a (Push _ s) = Push a s
-- -- modify _ Empty      = Empty

-- pop  :: Stack a -> Maybe (a, Stack a)
-- pop (Push a _ s) = Just (a, s)
-- pop _ = Nothing

-- isEmpty :: Stack a -> Bool
-- isEmpty Empty = True
-- isEmpty _ = False

-- peek :: Stack a -> Maybe (a, Stack a)
-- peek s@(Push a _ _) = Just (a, s)
-- peek _ = Nothing

-- length :: Stack a -> Int
-- length Empty = 0
-- length (Push _ n _) = n


module Stack where


type Stack a = [a]

push :: a -> Stack a -> Stack a
push a s = a:s

-- modify ::a -> Stack a -> Stack a
-- modify a (Push _ s) = Push a s
-- modify _ Empty      = Empty

pop  :: Stack a -> Maybe (a, Stack a)
pop []     = Nothing
pop (x:xs) = Just (x, xs) 

isEmpty :: Stack a -> Bool
isEmpty [] = True
isEmpty _  = False

peek :: Stack a -> Maybe (a, Stack a)
peek s@(x:xs) = Just (x, s)
peek _ = Nothing

-- length :: Stack a -> Int
-- length Empty = 0
-- length (Push _ n _) = n