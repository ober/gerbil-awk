GERBIL := gerbil
GAWK_BIN := gerbawk
GERBIL_HOME := $(shell $(GERBIL) config gerbil-home 2>/dev/null || echo $$HOME/.gerbil)

.PHONY: all build clean test install deps

all: build

deps:
	@mkdir -p .gerbil/pkg .gerbil/lib .gerbil/lib/static
	@# Link gerbil-pcre package if not already present
	@if [ ! -f .gerbil/pkg/gerbil-pcre.manifest ]; then \
		if [ -f $(HOME)/.gerbil/pkg/gerbil-pcre.manifest ]; then \
			cp -a $(HOME)/.gerbil/pkg/gerbil-pcre .gerbil/pkg/ 2>/dev/null || true; \
			cp $(HOME)/.gerbil/pkg/gerbil-pcre.manifest .gerbil/pkg/; \
		else \
			echo "Error: gerbil-pcre package not installed. Run: gerbil pkg install github.com/ober/gerbil-pcre2"; \
			exit 1; \
		fi; \
	fi
	@# Copy compiled lib artifacts if not present
	@if [ ! -d .gerbil/lib/gerbil-pcre ]; then \
		cp -r $(HOME)/.gerbil/lib/gerbil-pcre .gerbil/lib/ 2>/dev/null || true; \
	fi
	@# Copy static artifacts if not present
	@ls .gerbil/lib/static/gerbil-pcre__* >/dev/null 2>&1 || \
		cp $(HOME)/.gerbil/lib/static/gerbil-pcre__* .gerbil/lib/static/ 2>/dev/null || true

build: deps
	$(GERBIL) build

clean:
	rm -rf .gerbil $(GAWK_BIN)

test:
	$(GERBIL) test .

install: build
	install -m 755 .gerbil/bin/$(GAWK_BIN) /usr/local/bin/
