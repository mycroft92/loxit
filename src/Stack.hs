module Stack where
  

data Stack a = Empty | Push a (Stack a)

push :: a -> Stack a -> Stack a
push a s = Push a s

pop  :: Stack a -> Maybe (a, Stack a)
pop (Push a s) = Just (a, s)
pop _ = Nothing

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False
