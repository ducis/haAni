{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

module Interpolatable where
class Interpolatable p a where
	linInterp::p->a->a->a
	crInterp::p->a->a->a->a->a
instance Interpolatable p Float where
	linInterp p x0 x1 = x0*(1-p)+x1*(p)
	crInterp p xm1 x0 x1 x2 = x0 
instance (Interpolatable p a)=>Interpolatable p [a] where
	linInterp p x0 x1 = zipWith (linInterp p) x0 x1
	crInterp p xm1 x0 x1 x2 = x0
