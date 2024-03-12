module Stack where


data Stack a = Empty | Push a (Stack a)

push :: a -> Stack a -> Stack a
push = Push


-- modify ::a -> Stack a -> Stack a
-- modify a (Push _ s) = Push a s
-- modify _ Empty      = Empty

pop  :: Stack a -> Maybe (a, Stack a)
pop (Push a s) = Just (a, s)
pop _ = Nothing

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

peek :: Stack a -> Maybe (a, Stack a)
peek s@(Push a _) = Just (a, s)
peek _ = Nothing