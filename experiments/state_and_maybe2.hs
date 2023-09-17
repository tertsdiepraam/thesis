import Prelude hiding (Maybe,Just,Nothing,Left,Right)

newtype MS s a = MS (s -> Maybe (s, a))

instance Functor (MS s) where
  fmap :: (a -> b) -> MS s a -> MS s b
  fmap f (MS r) = MS $ \s -> 
    case r s of
      Nothing -> Nothing
      Just (s', a) -> Just (s', f a)

instance Applicative (MS s) where
  pure :: a -> MS s a
  pure a = MS $ \s -> Just (s, a)

  (<*>) :: MS s (a -> b) -> MS s a -> MS s b
  MS sf <*> MS sa = MS $
    \s ->
      case sf s of
        Nothing -> Nothing
        Just (s', f) -> case sa s' of
          Nothing -> Nothing
          Just (s'', a) -> Just (s'', f a)

instance Monad (MS s) where
  (>>=) :: MS s a -> (a -> MS s b) -> MS s b
  MS fa >>= k = MS $ \s ->
    case fa s of
      Nothing -> Nothing
      Just (s', a) ->
        let MS fb = k a
        in fb s'
    -- let (s', a) = fa s
    --     MS fb = k a
    --  in fb s'


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

get :: MS s s
get = MS $ \s -> Just (s, s)

put :: s -> MS s ()
put s = MS $ \_ -> Just (s, ())

runMS :: s -> MS s a -> Maybe (s, a)
runMS initialMS (MS func) = func initialMS

abort :: MS a b
abort = MS $ \_ -> Nothing

decrement1 :: MS Int Int
decrement1 = get >>= \s ->
               if s > 0
               then put (s - 1) >> return (s - 1)
               else abort
