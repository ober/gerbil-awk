# gerbil-gawk

AWK interpreter written in Gerbil Scheme, targeting GNU awk (gawk) compatibility.

## Build

- Binary: `gerbawk` (produced at `.gerbil/bin/gerbawk`)
- Build: `make build` (handles gerbil-pcre dependency setup automatically)
- Clean build: `make clean && make build`
- Install: `make install` (copies to `/usr/local/bin/`)
- Dependency: requires `gerbil-pcre` package installed globally (`gerbil pkg install github.com/ober/gerbil-pcre2`)

## MANDATORY: Verify Build Before Commit/Push

**NEVER commit or push without first verifying the build succeeds.** Always run:

```
make clean && make build
```

and confirm the binary works with a quick smoke test:

```
echo "hello world" | .gerbil/bin/gerbawk '{print $2}'
```

before ANY `git commit` or `git push`. This is non-negotiable. A broken build in the repo is unacceptable.

## Testing

Compare output against GNU awk (`gawk`):

```bash
expected=$(echo "input" | gawk 'program' 2>/dev/null)
actual=$(echo "input" | .gerbil/bin/gerbawk 'program' 2>/dev/null)
```

Hash iteration order may differ between gawk and gerbawk for `for (k in array)` — this is expected.

## Project Structure

- `awk/main.ss` — Entry point, tree-walking interpreter, eval-expr/exec-stmt
- `awk/parser.ss` — Recursive descent parser, tokenizer integration
- `awk/lexer.ss` — Tokenizer
- `awk/ast.ss` — AST node definitions (defstruct)
- `awk/value.ss` — AWK value type, number formatting (OFMT/%g/%f/%e), arithmetic ops
- `awk/runtime.ss` — Runtime environment (awk-env), field splitting, file/pipe I/O
- `awk/builtins/string.ss` — String builtins (sub, gsub, split, sprintf, etc.)
- `awk/builtins/math.ss` — Math builtins (sin, cos, sqrt, rand, etc.)
- `awk/builtins/io.ss` — I/O builtins (system, close, fflush)
- `awk/fields.ss` — Re-exports runtime
- `build.ss` — Build script with pcre2 linker flags
- `gerbil.pkg` — Package config, depends on gerbil-pcre

## Key Implementation Notes

- Uses `gerbil-pcre2` (package name `gerbil-pcre`) for regex, NOT `:std/pregexp`
- `(catch (pred? _) body)` — MUST use binding var `_` for predicate matching; `(catch (pred?) body)` is a catch-all
- `substring` requires 3 args in Gerbil: `(substring s start end)`
- Gambit `open-process` defaults all redirections to `#t` — set `stdout-redirection: #f` explicitly when needed
- `(floor +inf.0)` throws — guard with `(finite? n)` before calling `floor`
- Division by zero is a fatal error (matching gawk behavior)
