all: threefish_block.o idr_mash.o threefish_block.so idr_mash.so

threefish_block.o: threefish_block.c threefish_block.h
	gcc -I. -c threefish_block.c

threefish_block.so: threefish_block.c threefish_block.h
	gcc -I. -fPIC -o threefish_block.so -shared threefish_block.c

idr_mash.o: idr_mash.c idr_mash.h
	gcc -I. -c idr_mash.c

idr_mash.so: idr_mash.c idr_mash.h threefish_block.o
	gcc -I. -fPIC -o idr_mash.so -shared idr_mash.c threefish_block.o

clean:
	find . -name "*.o" -delete
	find . -name "*.so" -delete
