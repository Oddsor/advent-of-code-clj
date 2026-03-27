#!/usr/bin/env sh
clojure -Sdeps '{:deps {cljfmt/cljfmt {:mvn/version "0.9.2"}}}' \
  -M -m cljfmt.main $1
