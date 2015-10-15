isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing a = not (isJust a)

mabbe :: b -> (a -> b) -> Maybe a -> b
mabbe b f Nothing = b
mabbe b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a b = mabbe a id b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x = Just (head x)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just a):xs) = a: catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe list | hasNothing list = Nothing
               | otherwise = Just (catMaybes list)
               where hasNothing [] = False
                     hasNothing (Nothing:xs) = True
                     hasNothing ((Just a):xs) = hasNothing xs

lefts' :: [Either a b] -> [a]
lefts' list = catMaybes $ foldr ((:) . toMaybe) [] list
               where toMaybe (Left a) = Just a
                     toMaybe _ = Nothing

rights' :: [Either a b] -> [b]
rights' list = catMaybes $ foldr ((:) . toMaybe) [] list
                where toMaybe (Right a) = Just a
                      toMaybe _ = Nothing

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers list = (lefts' list, rights' list)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f val = either' (\x -> Nothing) (\x -> Just (f x)) val
