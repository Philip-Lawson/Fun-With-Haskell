-- This module is a little sandbox to practice 
-- implementing Monoid instances

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Nada 
  mappend (Only a) Nada = Nada
  mappend (Only a) (Only a') = Only $ mappend a a'

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

newtype Last' a =
  Last' { getLast' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Nada)) (First' (Nada)) = First' (Nada)
  mappend (First' (Only a)) (First' (Nada)) = First' (Only a)
  mappend (First' (Nada)) (First' (Only a)) = First' (Only a)
  mappend (First' (Only a)) (First' (Only a')) = First' (Only a)

instance Monoid (Last' a) where
  mempty = Last' Nada
  mappend (Last' Nada) (Last' (Only a)) = Last' (Only a)
  mappend (Last' (Only a)) (Last' Nada) = Last' (Only a)
  mappend (Last' (Only a)) (Last' (Only a')) = Last' (Only a')
