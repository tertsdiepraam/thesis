import Prelude hiding (Maybe,Just,Nothing,Left,Right)

newtype State s a = State (s -> (s, a))

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State r) = State $ \s -> let (s', a) = r s in (s', f a)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (,a)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State sf <*> State sa = State $
    \s ->
      let (s', f) = sf s
          (s'', a) = sa s'
       in (s'', f a)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State fa >>= k = State $ \s ->
    let (s', a) = fa s
        State fb = k a
     in fb s'


data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap f (Just a) = Just $ f a
  fmap _ Nothing  = Nothing

instance Applicative Maybe where
  pure = Just
    
  Just f <*> Just a = Just $ f a
  _ <*> _ = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a  >>= f = f a

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())

runState :: s -> State s a -> (s, a)
runState initialState (State func) = func initialState

abort :: Maybe a
abort = Nothing

decrement1 :: State Int (Maybe Int)
decrement1 = get >>= \s ->
               if s > 0
               then put (s - 1) >> return (return (s - 1))
               else return abort

-- decrement2 :: Maybe (State Int Int)
-- decrement2 = pure get >>= \st ->
--                 st >>= \x 
--                     if s > 0
--                     then pure (s >>= \s -> put (s - 1) >> get)
--                     else abort

decrement2 :: Maybe (State Int Int)
decrement2 = do
    let b = get >>= \s -> return (s - 1)
    if b
    then return (get >>= put (s - 1) >> get)
    else abort