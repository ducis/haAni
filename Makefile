player: player.hs HaAniRenderer.hs
	ghc -L. -lsqlite3 -O2 -threaded -rtsopts player.hs
test0: test0.hs testdb
	ghc -L. -lsqlite3 -O2 -threaded test0.hs
	./test0.exe 1280 720 33 test.db
testprivate: player
	./player.exe 1280 720 33 :memory: +RTS -N8 -RTS
testramdisk: player
	rm -f A:\\player.db
	./player.exe 1280 720 33 A:\\player.db +RTS -N8 -RTS
testhd: player
	rm -f player.db
	./player.exe 1280 720 33 player.db +RTS -N8 -RTS
testdb:
	rm -f test.db
	./sqlite3 test.db<sql_scripts/main.sql
	./sqlite3 test.db<animation.sql
#GTK = -m32 -IC:/Users/Karen/gtk+win32/include/gtk-2.0 -IC:/Users/Karen/gtk+win32/include/glib-2.0 -IC:/Users/Karen/gtk+win32/lib/glib-2.0/include -IC:/Users/Karen/gtk+win32/include/cairo -IC:/Users/Karen/gtk+win32/include/pango-1.0 -IC:/Users/Karen/gtk+win32/lib/gtk-2.0/include -IC:/Users/Karen/gtk+win32/include/gdk-pixbuf-2.0 -IC:/Users/Karen/gtk+win32/include/atk-1.0 -LC:/Users/Karen/gtk+win32/lib -lcairo -latk-1.0 -lgdk-win32-2.0 -lgio-2.0 -lglib-2.0 -lgtk-win32-2.0 -lgobject-2.0 -mms-bitfields -IC:/Users/Karen/gtk+win32/include/gtk-2.0 -IC:/Users/Karen/gtk+win32/lib/gtk-2.0/include -IC:/Users/Karen/gtk+win32/include/atk-1.0 -IC:/Users/Karen/gtk+win32/include/cairo -IC:/Users/Karen/gtk+win32/include/gdk-pixbuf-2.0 -IC:/Users/Karen/gtk+win32/include/pango-1.0 -IC:/Users/Karen/gtk+win32/include/glib-2.0 -IC:/Users/Karen/gtk+win32/lib/glib-2.0/include -IC:/Users/Karen/gtk+win32/include -IC:/Users/Karen/gtk+win32/include/freetype2 -IC:/Users/Karen/gtk+win32/include/libpng14

#controller:controller.cpp
#	gcc -mwindows -o controller.exe controller.cpp $(GTK) # `pkg-config --libs --cflags gtk+-2.0`
controller:controller.hs
	ghc -L. -lsqlite3 -O2 -threaded -rtsopts controller.hs
comb0: player controller
	rm -f A:\\player.db
	mintty -e bash -c "./player.exe 1280 720 33 A:\\player.db +RTS -N8 -RTS" &
	./controller A:\\player.db
monitor: monitor.hs
	ghc -L. -lsqlite3 -O2 -threaded -rtsopts monitor.hs
monitor0: monitor0.hs
	ghc -L. -lsqlite3 -O2 -threaded -rtsopts monitor0.hs
fulltest: player controller monitor
	rm -f A:\\player.db
	rm -f log.db
	mintty -e bash -c "./player.exe 1280 720 33 A:\\player.db +RTS -N8 -RTS" &
	mintty -e bash -c "./controller A:\\player.db" &
	#./monitor A:\\player.db log.db
	mintty -e bash -c "./monitor A:\\player.db log.db" &
