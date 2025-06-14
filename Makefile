OL=ol
PREFIX=~/.local

maybe_sqlite != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'

all: bin/nienor
bin/nienor: nienor.scm nienor/*.scm bin
	$(OL) -x c -o - nienor.scm | $(CC) -static -o bin/nienor -x c - $(maybe_sqlite) -lm -lpthread
bin:
	mkdir -p bin
clean:
	rm -f bin/nienor
install: bin/nienor
	cp -v bin/nienor $(PREFIX)/bin/nienor
uninstall:
	rm -v $(PREFIX)/bin/nienor
test: all
	$(OL) run-tests.scm
