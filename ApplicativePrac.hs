-- pure :: a -> f a
-- (<*>):: f (a -> b) -> f a -> f b
-- (<*>):: Either e (a -> b) -> Either e a -> Either e b

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success (f a)

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) _ (First a) = First a
  (<*>) (First a) _ = First a
  (<*>) (Second f) (Second a) = Second (f a) 

instance Monoid e =>
         Applicative (Validation e) where
   pure a = Success a
   (<*>) (Success f) (Success a) = Success (f a)
   (<*>) (Error e) (Error e') = Error (mappend e e')
   (<*>) (Error e) _ = Error e
   (<*>) _ (Error e) = Error e

-- exercise 1
newtype Identity a = Identity a deriving Show

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  (>>=) (Identity a) f = f a
  return a = Identity a

-- exercise 2
data Pair a = Pair a a deriving Show

instance Monoid a =>
          Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend (Pair a b) (Pair c d) = Pair (mappend a c) (mappend b d)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a a') = Pair (f a) (g a') 

-- exercise 3
data Two a b = Two a b
  deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' b) = Two (mappend a a') (f b)

-- exercise 4
data Three a b c = Three a b c 
  deriving Show

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (mappend a a') (mappend b b') (f c)

-- exercise 5
data Three' a b = Three' a b b
  deriving Show

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f g) (Three' a' b c) = Three' (mappend a a') (f b) (g c)

-- exercise 6
data Four a b c d = Four a b c d
  deriving Show

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' d) =
                                  Four (mappend a a') (mappend b b') (mappend c c') (f d)

-- exercise 7
data Four' a b = Four' a a a b
  deriving Show

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (mappend a a') (mappend b b') (mappend c c') (f d)
