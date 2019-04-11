data MyMaybe a = MyJust a | Nuttin deriving (Eq, Show)

instance Functor MyMaybe where
fmap f (MyJust x) = MyJust (f x)
fmap _ _ = Nuttin

instance Applicative MyMaybe where
pure = MyJust
Nuttin <*> _ = Nuttin
(MyJust f) <*> something = fmap f something

instance Monad MyMaybe where
return = MyJust
Nuttin >>= _ = Nuttin
MyJust x >>= f = f x
