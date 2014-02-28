define hs-bin
./hs/dist/build/haAni-$(1)/haAni-$(1)
endef
WINRD=\#You need to create ramdisk A: manually
export PLAY=$(call hs-bin,player)
export CONTROL=$(call hs-bin,controller)
export MONITOR=$(call hs-bin,monitor)
WINTERM=mintty -e bash -c
hs: .FRC
	cd hs && cabal install --only-dependencies && cabal configure && cabal build
hs-clean: .FRC
	cd hs && cabal clean
hs-install: .FRC
	cd hs && cabal install
test-gl: .FRC
	$(call hs-bin,gltest) 1280 720 33 test.db
test-private: .FRC
	$(PLAY) 1280 720 33 :memory: +RTS -N8 -RTS
test-stdfile: .FRC
	rm -f player.db
	$(PLAY) 1280 720 33 player.db +RTS -N8 -RTS
test-ramdisk-win: .FRC
	$(WINRD)	
	rm -f A:\\player.db
	$(PLAY) 1280 720 33 A:\\player.db +RTS -N8 -RTS
test-shm: .FRC
	rm -f /dev/shm/player.db
	$(PLAY) 1280 720 33 /dev/shm/player.db +RTS -N8 -RTS
testdb: .FRC
	rm -f test.db
	./sqlite3 test.db<sql_scripts/main.sql
	./sqlite3 test.db<animation.sql
ctrl-test-win: .FRC
	$(CONTROL) player.db
comb0-win: .FRC
	$(WINRD)
	rm -f A:\\player.db
	$(WINTERM) "./player.exe 1280 720 33 A:\\player.db +RTS -N8 -RTS" &
	$(CONTROL) A:\\player.db
fulltest: .FRC
	#PLEASE manually set $$TMP to /dev/shm under linux or to some ramdisk under windows.
	#Using the default $$TMP under cygwin probably leads to failure.
	echo ${TMP}
	rm -f ${TMP}/player.db
	rm -f log.db
	screen -c screen-fulltest
fulltest-win: .FRC
	$(WINRD)
	rm -f A:\\player.db
	rm -f log.db
	$(WINTERM) "$(PLAY) 1280 720 33 A:\\player.db +RTS -N8 -RTS" &
	$(WINTERM) "$(CONTROL) A:\\player.db animation.sql" &
	$(WINTERM) "$(MONITOR) A:\\player.db log.db" &

.FRC:
