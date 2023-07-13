#!/usr/bin/env sh
clojure -Sdeps '{:deps {cljfmt/cljfmt {:mvn/version "0.9.2"}}}' \
  -m cljfmt.main $1