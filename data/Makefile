.PHONY: deploy clean preview

deploy: clean build
	cp -pr ./_site/* ../
	cp -pr ./css/ ../css

clean: gh-pages
	./gh-pages clean

preview: deploy
	open ../index.html

build: gh-pages
	./gh-pages build

gh-pages: gh-pages.hs
	ghc --make gh-pages.hs
