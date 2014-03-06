SRC = $(wildcard *.hs)

all: main

clean:
	rm -f *.o
	rm -f *.hi

main: $(SRC)
	ghc --make -Wall $^

.PHONY: all clean
