#!/usr/bin/env bash
# ---- Local check for a positionR package ----
# Usage: tools/check_all.sh [package-dir]
#   package-dir defaults to the folder above tools/.
# Steps: deps, document, build, check, lint, gates, shared (STEPS below).
# Every step runs even after a failure, so one run shows every problem; the
# exit status is 1 when any step failed.
# Build and check output go to a temporary folder outside the package
# folder, named on the last line. To use packages from another library, such as lintr, run
# R_LIBS=/path/to/library tools/check_all.sh.

STEPS=(deps document build check lint gates shared)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="$(cd "${1:-$SCRIPT_DIR/..}" && pwd)"
ROOT="$(dirname "$PKG_DIR")"
PKG="$(Rscript -e "cat(read.dcf('$PKG_DIR/DESCRIPTION', 'Package'))")"
VER="$(Rscript -e "cat(read.dcf('$PKG_DIR/DESCRIPTION', 'Version'))")"
OUT="$(mktemp -d "${TMPDIR:-/tmp}/check_${PKG}.XXXXXX")"
LOG="$OUT/check_all.log"
TARBALL="$OUT/${PKG}_${VER}.tar.gz"
FAILED=""

# Sibling packages this one imports are installed here, not in the user's
# library. The folder goes first on the library path of every step.
mkdir -p "$OUT/lib"
export R_LIBS="$OUT/lib${R_LIBS:+:$R_LIBS}"

say() { printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "$LOG"; }

run_step() {
  local name="$1" t0 status
  say "START $name"
  t0=$(date +%s)
  "step_$name" 2>&1 | tee -a "$LOG"
  status=${PIPESTATUS[0]}
  say "DONE $name in $(( $(date +%s) - t0 )) s, exit $status"
  if [ "$status" -ne 0 ]; then
    say "FAIL $name"
    FAILED="$FAILED $name"
  fi
}

step_deps() {
  cat > "$OUT/deps.R" <<'RS'
args <- commandArgs(TRUE)
pkg_dir <- args[1]
lib <- args[2]
fields <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), fields = c("Depends", "Imports", "Suggests"))
parse_deps <- function(txt) {
  v <- unlist(strsplit(paste(txt[!is.na(txt)], collapse = ","), ","))
  v <- unique(trimws(sub("\\(.*$", "", v)))
  setdiff(v[nzchar(v)], c("R", rownames(installed.packages(priority = "base"))))
}
needed <- parse_deps(fields[, c("Depends", "Imports")])
suggested <- setdiff(parse_deps(fields[, "Suggests"]), needed)
deps <- c(needed, suggested)
have <- function(p) nzchar(system.file(package = p))
missing <- deps[!vapply(deps, have, logical(1))]
cat(sprintf("deps: %d declared (Depends, Imports, Suggests); %d missing%s\n", length(deps),
            length(missing), if (length(missing)) paste0(": ", paste(missing, collapse = ", ")) else ""))
# Siblings are installed in the order they depend on one another, whatever
# the order DESCRIPTION lists them in: arraydesign imports detfield, so
# detfield goes in first.
sibling_order <- c("detfield", "detqc", "arraydesign", "arraydesignR")
missing <- c(intersect(sibling_order, missing), setdiff(missing, sibling_order))
for (p in missing) {
  src <- file.path(dirname(pkg_dir), p)
  if (!file.exists(file.path(src, "DESCRIPTION"))) next
  cat(sprintf("deps: installing sibling %s into %s\n", src, lib))
  system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load",
          paste0("--library=", shQuote(lib)), shQuote(src)), stdout = FALSE, stderr = FALSE)
}
still <- missing[!vapply(missing, have, logical(1))]
hard <- intersect(still, needed)
soft <- intersect(still, suggested)
# A missing Suggests package is a warning: the tests and vignettes that use
# it skip. A missing Depends or Imports package fails the step.
if (length(soft)) {
  cat(sprintf("deps: warning: Suggests not installed: %s; the tests that need them skip\n",
              paste(soft, collapse = ", ")))
}
if (length(hard)) {
  cat(sprintf("deps: still missing (Depends, Imports): %s\n", paste(hard, collapse = ", ")))
  cat(sprintf("deps: install with: Rscript -e 'install.packages(c(%s))'\n",
              paste0('"', hard, '"', collapse = ", ")))
  quit(status = 1)
}
RS
  Rscript "$OUT/deps.R" "$PKG_DIR" "$OUT/lib"
}

step_document() {
  Rscript -e "devtools::document('$PKG_DIR')"
}

step_build() {
  (cd "$OUT" && R CMD build --no-manual "$PKG_DIR")
}

step_check() {
  local status line
  (cd "$OUT" && R CMD check --as-cran --no-manual "$TARBALL")
  status=$?
  line="$(grep '^Status:' "$OUT/$PKG.Rcheck/00check.log" 2>/dev/null)"
  echo "check: ${line:-no Status line in 00check.log}"
  if [ "$status" -ne 0 ] || [ -z "$line" ] || echo "$line" | grep -Eq 'ERROR|WARNING'; then
    return 1
  fi
}

# lintr resolves internals, imports and declared globals from the installed
# namespace, so the package is installed into $OUT/lib (first on R_LIBS)
# before it is linted; without this it reports every cross-file call.
step_lint() {
  if ! R CMD INSTALL --no-test-load --library="$OUT/lib" "$TARBALL" > "$OUT/lint_install.log" 2>&1; then
    echo "lint: could not install $TARBALL; see $OUT/lint_install.log"
    return 1
  fi
  Rscript -e "lints <- lintr::lint_package('$PKG_DIR'); print(lints);
    cat(length(lints), 'lints\n'); quit(status = as.integer(length(lints) > 0))"
}

step_gates() {
  Rscript "$PKG_DIR/tools/check_gates.R" "$PKG_DIR" --tarball="$TARBALL"
}

# The contract and the tools are copied into each package, because each is
# its own repository. Copies that drift apart fail here.
step_shared() {
  local status=0 sib f
  for sib in detfield detqc arraydesign arraydesignR; do
    [ "$sib" = "$PKG" ] && continue
    if [ ! -d "$ROOT/$sib/tools" ]; then
      echo "shared: $sib not found beside $PKG; tools not compared"
      continue
    fi
    for f in CONTRACT.md tools/check_all.sh tools/check_gates.R .lintr \
             tools/purl_vignettes.R; do
      # arraydesignR holds no purl_vignettes.R, so that file is compared only
      # where both packages hold it.
      if [ "$f" = tools/purl_vignettes.R ] &&
         { [ ! -f "$PKG_DIR/$f" ] || [ ! -f "$ROOT/$sib/$f" ]; }; then
        echo "shared: $f not in both $PKG and $sib; not compared"
        continue
      fi
      if cmp -s "$PKG_DIR/$f" "$ROOT/$sib/$f"; then
        echo "shared: $f matches $sib"
      else
        echo "shared: $f differs from $sib"
        status=1
      fi
    done
  done
  # The figure theme lives in detfield; detqc does not import detfield and
  # holds a copy. Each package compares the two copies beside it.
  local theme=R/theme_positionr.R
  if [ ! -f "$ROOT/detfield/$theme" ] || [ ! -f "$ROOT/detqc/$theme" ]; then
    for sib in detfield detqc; do
      [ -f "$ROOT/$sib/$theme" ] || echo "shared: $sib/$theme not found"
    done
    case "$PKG" in detfield|detqc) status=1 ;; esac
  elif cmp -s "$ROOT/detfield/$theme" "$ROOT/detqc/$theme"; then
    echo "shared: $theme matches between detfield and detqc"
  else
    echo "shared: $theme differs between detfield and detqc"
    status=1
  fi
  return $status
}

say "check_all: $PKG $VER in $PKG_DIR; output in $OUT"
for step in "${STEPS[@]}"; do
  run_step "$step"
done
n_failed=$(echo "$FAILED" | wc -w | tr -d ' ')
say "SUMMARY $PKG: ${#STEPS[@]} steps, $n_failed failed${FAILED:+:$FAILED}; output in $OUT"
[ "$n_failed" -eq 0 ]
