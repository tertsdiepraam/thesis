{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
import Prelude hiding (Maybe,Just,Nothing,Left,Right)

data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap f (Just a) = Just $ f a
  fmap _ Nothing  = Nothing

data Free f a
  = Pure a
  | Do (f (Free f a))

fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
fold gen _   (Pure x) = gen x
fold gen alg (Do f)   = alg (fmap (fold gen alg) f)

instance Functor f => Monad (Free f) where
  m >>= k = fold k Do m

instance Functor f => Functor (Free f) where
  fmap f = fold (pure . f) Do

instance Functor f => Applicative (Free f) where
  pure = Pure
  f <*> m = fold (`fmap` m) Do f

data Abort k = Abort
  deriving Functor

data State s k = Put s k | Get (s -> k)
  deriving Functor

--
data End k
  deriving Functor

infixr 6 +
data (f + g) a = L (f a) | R (g a)
  deriving Functor

class f < g where
  inj :: f k -> g k

instance f < f where inj = id
instance f < (f + g) where inj = L
instance f < h => f < (g + h) where inj = R . inj

get :: State s < f => Free f s
get = Do $ inj $ Get Pure

put :: State s < f => s -> Free f ()
put s = Do $ inj $ Put s $ Pure ()

abort :: Abort < f => Free f a
abort = Do $ inj Abort

handle :: (Functor f, Functor f')
       => (a -> p -> Free f' b)
       -> (f (p -> Free f' b) -> p -> Free f' b)
       -> Free (f + f') a -> p -> Free f' b
handle ret f = fold ret $
  \case
    L x -> f x
    R x -> \p -> Do $ fmap (\m -> m p) x

decrement :: (Functor f, Abort < f, State Int < f) => Free f Int
decrement = get >>= \s ->
              if s > (0::Int)
              then put (s - 1) >> get
              else abort

handleEnd :: Free End a -> a
handleEnd (Pure a) = a

handleAbort :: Functor f => Free (Abort + f) a -> Free f (Maybe a)
handleAbort c = handle (\a _ -> Pure $ Just a) (\Abort () -> Pure Nothing) c ()

handleState :: Functor f => s -> Free (State s + f) a -> Free f (s, a)
handleState = flip $ handle
  (\x s -> pure (s, x))
  (\x s -> case x of
      Put s' k -> k s'
      Get k -> k s s)

result :: (Int, Maybe Int)
result = handleEnd $ handleState (0::Int) $ handleAbort decrement

data Except a
  = Throw String
  | Catch a a

instance Functor Except where
  fmap :: (a -> b) -> Except a -> Except b
  fmap f (Throw s) = Throw s
  fmap f (Catch a b) = Catch (f a) (f b)

throw :: String -> Free Except a
throw s = Do $ Throw s

catch :: Free Except a -> Free Except a -> Free Except a
catch fa fb = Do $ Catch fa fb

a = pure 5
b = pure 6
k = \_ -> throw "Error"

x = catch a b >>= k

x' = Do (Catch a b) >>= k

x'' = Do $ fmap (>>= k) (Catch a b)

x''' = Do $ Catch (a >>= k) (b >>= k)

x'''' = Do $ Catch (pure 5 >>= \_ -> throw "Error") (pure 6 >>= \_ -> throw "Error")

x''''' = Do $ Catch (throw "Error") (throw "Error")

