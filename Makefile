CFILES = $(wildcard src/*.cpp)
OFILES = $(patsubst src/%.cpp,build/%.o,${CFILES})

FLAGS = -O3 -Wall -Wextra -std=c++20

CPP = g++

all: build/burlap
debug: FLAGS += -g -DDEBUG -O0
debug: all

build/%.o: src/%.cpp
	@mkdir -p ./build
	${CPP} ${FLAGS} -c $< -o $@

build/burlap: ${OFILES}
	${CPP} ${OFILES} -o build/burlap

clean:
	rm -rf build
