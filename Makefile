.PHONY: all clean preview

all:
	cd orig_data; make deploy

clean:
	cd orig_data; make clean

preview:
	cd orig_data; make preview
