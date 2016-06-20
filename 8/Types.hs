module Tofu where

    data Frank a b  = Frank {frankField :: b a} deriving (Show)

    class Tofu t where
        tofu :: j a -> t a j

    instance Tofu Frank where
        tofu = Frank

    data Barry t k p = Barry { yabba :: p, dabba :: t k }

    instance Functor (Barry a b) where
        fmap f Barry { yabba = x, dabba = y } = Barry { yabba = f x, dabba = y }
