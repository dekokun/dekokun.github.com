.PHONY: deploy clean preview compile

deploy: clean build
	cp -pr _site/index.html ./index.html
	cp -pr _site/data/css/ ./css
	cp -pr _site/data/posts/ ./posts
	cp -pr _site/posts.html ./posts.html

clean:
	./gh-pages clean

preview: deploy
	open index.html

build:
	./gh-pages build

compile:
	ghc --make gh-pages.hs
