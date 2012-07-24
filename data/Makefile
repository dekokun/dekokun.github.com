.PHONY: deploy clean preview

deploy: clean build
	cp -pr ./_site/index.html ../index.html
	cp -pr ./css/ ../css
	cp -pr ./_site/posts/ ../posts
	cp -pr ./_site/posts.html ../posts.html

clean: gh-pages
	./gh-pages clean

preview: deploy
	open ../index.html

build: gh-pages
	./gh-pages build

gh-pages: gh-pages.hs
	ghc --make gh-pages.hs
