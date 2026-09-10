#!/usr/bin/env bash
# Safe wrapper around `quarto render`.
#
# Why this exists: a failed render leaves _docs/ WIPED, not stale. Since _docs
# is what Netlify publishes, a bad render mid-semester takes the site down until
# someone notices. This checks the things that actually break the build, then
# restores _docs from git if the render fails anyway.
#
# Usage:  ./render.sh            (whole site)
#         ./render.sh content/Week_05/05b.qmd
set -uo pipefail
cd "$(dirname "$0")"

FRAGILE=(assignment/05-assignment.qmd content/Week_05/05b.qmd
         content/Week_13/13a.qmd content/Week_15/15a.qmd)

echo "==> preflight"

# 1. Census API key, but only complain if a file that needs it will re-execute.
need_key=0
for f in "${FRAGILE[@]}"; do
  if ! git diff --quiet HEAD -- "$f" 2>/dev/null; then need_key=1; echo "    modified: $f"; fi
done
if [ "$need_key" = "1" ]; then
  if Rscript --vanilla -e 'readRenviron("~/.Renviron"); quit(status = if (nzchar(Sys.getenv("CENSUS_API_KEY"))) 0 else 1)' 2>/dev/null; then
    echo "    Census API key: found"
  else
    echo "    ERROR: you edited a file that makes live Census calls, but no CENSUS_API_KEY is set."
    echo "           Rendering it will fail and wipe _docs/."
    echo "           Fix: Rscript -e 'tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE)'"
    echo "           Free key: https://api.census.gov/data/key_signup.html"
    exit 1
  fi
else
  echo "    Census API key: not needed (no live-call file modified)"
fi

# 2. Packages that are easy to miss because nothing calls library() on them.
missing=$(Rscript --vanilla -e '
p <- c("tidyverse","data.table","here","knitr","rmarkdown","pander","sf","tigris",
       "tidycensus","caret","glmnet","rpart","rpart.plot","maps","mapproj","rafalib",
       "dslabs","Lahman","broom","skimr","kableExtra","jsonify","arcpbf")
m <- p[!sapply(p, requireNamespace, quietly = TRUE)]
cat(paste(m, collapse = " "))' 2>/dev/null)
if [ -n "$missing" ]; then
  echo "    ERROR: missing R packages: $missing"
  echo "           install.packages(c($(echo "$missing" | sed 's/ /", "/g; s/^/"/; s/$/"/')))"
  exit 1
fi
echo "    R packages: all present"

before=$(find _docs -name '*.html' ! -name '* [0-9].html' 2>/dev/null | wc -l | tr -d ' ')
echo "    _docs currently has $before html pages"

echo "==> quarto render ${*:-（whole site）}"
if quarto render "$@"; then
  after=$(find _docs -name '*.html' ! -name '* [0-9].html' | wc -l | tr -d ' ')
  echo "==> OK. _docs now has $after html pages (was $before)"
  if [ "$after" -lt "$before" ]; then
    echo "    WARNING: page count dropped. Check that's intended before committing."
  fi
else
  echo "==> RENDER FAILED"
  after=$(find _docs -name '*.html' ! -name '* [0-9].html' 2>/dev/null | wc -l | tr -d ' ')
  echo "    _docs has $after html pages; restoring from git..."
  git checkout -- _docs && echo "    _docs restored ($(find _docs -name '*.html' ! -name '* [0-9].html' | wc -l | tr -d ' ') pages). Site is safe."
  echo "    Scroll up for the actual error."
  exit 1
fi
