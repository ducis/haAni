import Graphics.Gloss
import System.Random
import Data.List
import Interpolatable
--main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--main = play (InWindow "Tower Defense m7" (800, 600) (10, 10)) white 32 wInit simplyDraw (\e w->catchInput (eventConv e) w) (\t w->stepW (wpar 3) w)
--simplyDraw w = Translate (-400) 0 $ Scale 0.1 0.1 $ Text $ show w
--eventConv e = I
main = simulate (InWindow "haAni" (1280,720) (10,10)) white 30 initialModel visualize (\vp->step)
initialModel = M 0 0 $ (\x->[x,x,x]) [[0.5+i*0.01, 0.5+i*0.001, i*0.001, i*0.002, i*0.005]|i<-[1..4]]
data Model = M T Integer [[[Float]]]
type T = R
type R = Float
step dt (M t i [cs0,cs1,cs]) = M (t+dt) (i+1) 
	$ if j==0
	then [cs1,(map (map (ds0 4)) cs1),cs1]
	else [cs0,cs1,linInterp ((fromIntegral j::Float)/fromIntegral n) cs0 cs1 ]
	where 
	j = i `mod` n
	n = 15
visualize (M t i [_,_,cs]) = Pictures $ map (\[x,y,r,d,a]->Translate x y (clip0 2 (r*120) (2+d*10) (50-d*30) (12-r*10) (80+r*40) (a*360))) $ map (\(x:y:m)->((x-0.5)*640):((y-0.5)*360):m) cs
	

clip0 n r0 t0 r1 t1 e t = Rotate t $ Pictures ((Color (makeColor 0.1 0.5 0 0.8) $ ThickCircle r0 t0):map (\a->Color (makeColor 0.5 0.5 0 0.5) $ Rotate a (Translate e 0 $ ThickCircle r1 t1)) [i*360/n|i<-[1..n]])
ds0 b x = b*x*(1-x)

