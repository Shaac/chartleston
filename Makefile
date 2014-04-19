TARGET = chartleston
SRC = $(wildcard *.hs)

all: $(TARGET)

clean:
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGET)

$(TARGET): $(SRC)
	ghc --make -Wall $^

.PHONY: all clean
