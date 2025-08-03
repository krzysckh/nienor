OL=ol
PREFIX=~/.local
OL_WIN=ol-small.exe

maybe_sqlite != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'

all: bin/nienor
bin/nienor: nienor.scm nienor/*.scm nienor/lib/*.scm bin
	$(OL) -x c -o - nienor.scm | $(CC) -static -o bin/nienor -x c - $(maybe_sqlite) -lm -lpthread
bin/nienor.exe: nienor.scm nienor/*.scm nienor/lib/*.scm bin
	wine $(OL_WIN) -x c -o - nienor.scm | i686-w64-mingw32-gcc -static -o bin/nienor.exe -I/usr/local/include -x c - -lm -lopengl32 -lwinmm -lgdi32 -lws2_32
bin:
	mkdir -p bin
clean:
	rm -f bin/nienor
install: bin/nienor
	cp -v bin/nienor $(PREFIX)/bin/nienor
uninstall:
	rm -v $(PREFIX)/bin/nienor
uxncli.c:
	wget https://git.sr.ht/~rabbits/uxn11/blob/f366f0db973b2a9ee2c9b19df02a5d6951072b32/src/uxncli.c
uxncli: uxncli.c
	$(CC) -O2 -o uxncli uxncli.c
test:
	$(OL) run-tests.scm
