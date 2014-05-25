TARGET = chartleston
SRC = $(wildcard src/*.hs)

all: $(TARGET)

clean:
	rm -f src/*.o
	rm -f src/*.hi
	rm -f $(TARGET)

$(TARGET): $(SRC)
	ghc --make -Wall $^ -o $(TARGET)

.PHONY: all clean
