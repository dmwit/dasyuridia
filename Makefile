DASYURIDIA_BIN=$(shell cabal list-bin dasyuridia)

all: lib/poll_stdin.so $(DASYURIDIA_BIN)

lib/poll_stdin.so: lib/poll_stdin.c
	gcc $(shell pkg-config --cflags lua-5.1) $^ -shared -fPIC -o $@

$(DASYURIDIA_BIN): app/Main.hs dasyuridia.cabal
	cabal build exe:dasyuridia
	touch $(DASYURIDIA_BIN) # just in case cabal determines nothing needs to be done

clean:
	@echo "This target doesn't work, because I couldn't decide whether it ought to delete"
	@echo "dist-newstyle or not. To avoid disagreements between past me and stupid"
	@echo "forgetful future me, I therefore made two targets: cleanish and very_clean. Try"
	@echo "one of those."

cleanish:
	-rm lib/poll_stdin.so

very_clean: cleanish
	-rm -r dist-newstyle

.PHONY: clean cleanish very_clean all
