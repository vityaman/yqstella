.DEFAULT_GOAL := fizruk

STACK ?= stack
HPACK ?= hpack

.PHONY: \
	ci \
	tooling \
	generate \
	build \
	test \
	format \
	style \
	install-hpack \
	install-bnfc \
	generate-syntaxgen-module \
	generate-yson-syntaxgen-module \
	generate-cabal-file \
	build-dependencies \
	build-project \
	install-ormolu \
	check-haskell-formatting \
	install-hlint \
	check-haskell-style

fizruk: generate build

ci: generate build test format style

tooling: install-hpack install-bnfc install-ormolu install-hlint

generate: install-hpack install-bnfc generate-syntaxgen-module generate-yson-syntaxgen-module generate-cabal-file

build: build-dependencies build-project

test:
	$(STACK) test

format: install-ormolu check-haskell-formatting

style: install-hlint check-haskell-style

install-hpack:
	$(STACK) install hpack

install-bnfc:
	$(STACK) install BNFC
	$(STACK) install alex
	$(STACK) install happy

generate-syntaxgen-module:
	./syntax/stella/codegen.bash

generate-yson-syntaxgen-module:
	./syntax/yson/codegen.bash

generate-cabal-file:
	$(HPACK)

build-dependencies:
	$(STACK) build --only-dependencies

build-project:
	$(STACK) build

install-ormolu:
	$(STACK) install ormolu

check-haskell-formatting:
	$(STACK) exec ormolu -- --mode check $$(find . -path './src/*.hs' -not -path './src/*SyntaxGen/*')
	$(STACK) exec ormolu -- --mode check $$(find . -path './test/*.hs')

install-hlint:
	$(STACK) install hlint

check-haskell-style:
	$(STACK) exec hlint -- .
