module Lisp where

import Control.Monad

data Elem = Cons Elem Elem
          | Null
          | Symbol String
          | Integer Integer
                   
makeList :: [Elem] -> Elem
makeList = foldr Cons Null

fromList :: Elem -> [Elem]
fromList = foldrList (:) []

foldlList f x0 (Cons x xs) = foldlList f (f x0 x) xs
foldlList _ x0 Null        = x0

foldrList f xn (Cons x xs) = f x (foldrList f xn xs)
foldrList _ xn Null        = xn


(%%) :: Elem -> Elem -> Elem
(%%) = Cons

{-
instance Functor Elem where
 
 fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Null        = Null
-}

instance Show Elem where
  show (Symbol x) = x
  show (Integer x) = show x
  show Null        = "'()"
  show (Cons x y) = "("
                    ++ showList (Cons x y)
                    ++ ")"
    where showList (Cons x Null) = show x
          showList (Cons x (Cons y z)) =
            show x ++ " " ++ showList (Cons y z)
          showList (Cons x y) = show x ++ " . " ++ showList y
          showList Null       = ""
          showList x = show x
