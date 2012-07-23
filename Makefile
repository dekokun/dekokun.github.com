.PHONY: deploy clean preview compile

deploy: clean build
	mv _site/index.html ./index.html
	mv _site/data/css ./css
	mv _site/data/posts ./posts
	mv _site/data/posts.html ./posts.html

clean:
	./gh-pages clean

preview: deploy
	open index.html

build:
	./gh-pages build

compile:
	ghc --make gh-pages.hs
