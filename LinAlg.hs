{-# LANGUAGE FlexibleInstances #-}
module LinAlg where 
import Data.Array
instance (Ix i, Show i, Num e)=>Num (Array i e) where
	a+b = listArray (bounds a) (zipWith (+) (elems a) (elems b))
	a*b =
