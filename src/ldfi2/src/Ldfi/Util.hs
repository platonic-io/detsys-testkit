module Ldfi.Util where

infixr 9 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

infixr 9 .::

(.::) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.::) = (.:) . (.:)
