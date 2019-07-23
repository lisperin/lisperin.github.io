#!/bin/bash

OUT="$1"

echo "Generating $OUT"

cat presentation.tmpl.html | PRESENTATION=$(cat presentation.md | recode ascii..html) /usr/local/opt/gettext/bin/envsubst >"$OUT"
