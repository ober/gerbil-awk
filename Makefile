GERBIL := gerbil
GAWK_BIN := gerbawk
GERBIL_HOME := $(shell $(GERBIL) config gerbil-home 2>/dev/null || echo $$HOME/.gerbil)

# Static binary build variables
ARCH := $(shell uname -m)
PWD := $(shell pwd)
DOCKER_IMAGE := "gerbil/gerbil:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)

.PHONY: all build clean test install deps lib install-lib uninstall-lib static build-static

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

# --- Library target ---
# Compile modules without linking the executable.
# Other projects can then import :gerbil-awk/awk/lib

lib: deps
	GERBAWK_LIB_ONLY=1 $(GERBIL) build

install-lib: lib
	@echo "Installing gerbil-awk library via gxpkg link..."
	@cd $(HOME) && gxpkg link gerbil-awk $(CURDIR) || true
	@cd $(HOME) && gxpkg build gerbil-awk
	@echo "Installed. Other projects can now: (import :gerbil-awk/awk/lib)"

uninstall-lib:
	@cd $(HOME) && gxpkg unlink gerbil-awk || true

# --- Static binary target ---
# Builds a fully static gerbawk binary using Docker + Alpine/musl

static: clean
	docker run --rm \
	  --ulimit nofile=8192:8192 \
	  -v $(PWD):/src:z \
	  $(DOCKER_IMAGE) \
	  sh -c "apk add --no-cache pcre2-dev pcre2-static su-exec && \
	         chown -R $(UID):$(GID) /opt/ && \
	         mkdir -p /tmp/gerbawk-build && chown $(UID):$(GID) /tmp/gerbawk-build && \
	         exec su-exec $(UID):$(GID) env HOME=/tmp/gerbawk-build sh -c '\
	           cd /src && \
	           make build-static && \
	           echo \"Static build complete: .gerbil/bin/$(GAWK_BIN)\"'"

build-static: deps
	GERBAWK_STATIC=1 $(GERBIL) build
