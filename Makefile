default: build

.PHONY: help
help: # Show help for each of the Makefile recipes.
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done

.PHONY: build-clerk
build-clerk: # Build clerk notebook
	clojure -X:buildclerk

.PHONY: build-clay
build-clay: # Build clay notebook
	clojure -X:buildclay

.PHONY: deps-list
deps-list: # List dependencies
	clojure -X:deps list

.PHONY: deps-tree
deps-tree: # List dependencies
	clojure -X:deps tree

.PHONY: update-deps
update-deps: # Update dependencies
	clojure -M:update

.PHONY: lint
lint: # Build and run jar-file
	clj-kondo --lint src --lint test
	#clj -Sdeps '{:deps {clj-kondo/clj-kondo {:mvn/version "RELEASE"}}}' -M -m clj-kondo.main --lint src --lint test

.PHONY: test
test: # Run tests
	bin/kaocha

.PHONY: docker-clay
docker-clay: # Build clay using docker image
	docker run --rm -v $(PWD):/usr/src/app -w /usr/src/app -e INPUT_SALT=$(INPUT_SALT) docker.io/clojure:tools-deps clj -X:buildclay

.PHONY: docker-clay
docker-clerk: # Build clay using docker image
	docker run --rm -v $(PWD):/usr/src/app -w /usr/src/app -e INPUT_SALT=$(INPUT_SALT) docker.io/clojure:tools-deps clj -X:buildclerk
