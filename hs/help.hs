main = putStrLn $ unlines $ [
	"haAni-gltest 1280 720 33 test.db",
	"haAni-player 1280 720 33 :memory: +RTS -N8 -RTS",
	"haAni-player 1280 720 33 player.db +RTS -N8 -RTS"]
