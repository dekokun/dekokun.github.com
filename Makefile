.PHONY: all clean preview

all:
	cd data; make deploy

clean:
	cd data; make clean

preview:
	cd data; make preview
