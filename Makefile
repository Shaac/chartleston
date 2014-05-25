TARGET = chartleston
SRC = $(wildcard src/*.hs)
GHCFLAGS = -Wall -Werror -O

all: $(TARGET)

clean:
	rm -f src/*.o
	rm -f src/*.hi
	rm -f $(TARGET)

$(TARGET): $(SRC)
	ghc --make $(GHCFLAGS) $^ -o $@

.PHONY: all clean
