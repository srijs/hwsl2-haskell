TARGET = coholos.so
LIBS =
CC = clang
CFLAGS = -Wall -msse2 -msse4.1 -mpclmul -O3

.PHONY: default all clean test

default: $(TARGET)
all: default

OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
HEADERS = $(wildcard *.h)
TESTS = $(wildcard tests/*.c)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
	$(CC) -shared $(OBJECTS) -Wall $(LIBS) -o $@

test: $(OBJECTS) $(TESTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(TESTS) -o test
	./test

clean:
	-rm -f *.o
	-rm -f $(TARGET)
	-rm -f test
