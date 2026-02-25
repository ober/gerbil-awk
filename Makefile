GERBIL := gerbil
GAWK_BIN := gawk

.PHONY: all build clean test install

all: build

build:
	$(GERBIL) build

clean:
	rm -rf .gerbil $(GAWK_BIN)

test:
	$(GERBIL) test .

install: build
	install -m 755 $(GAWK_BIN) /usr/local/bin/
