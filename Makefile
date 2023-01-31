CFILES = $(wildcard src/*.cpp)
OFILES = $(patsubst src/%.cpp,build/%.o,${CFILES})

FLAGS = -O3 -Wall -Wextra -std=c++20 `pkg-config --cflags libedit`

CPP = g++

all: rust build/burlap
debug: FLAGS += -g -DDEBUG -O0
debug: all

build/%.o: src/%.cpp
	@mkdir -p ./build
	${CPP} -c $< -o $@ ${FLAGS}

build/burlap: ${OFILES}
	${CPP} ${OFILES} -ledit -o build/burlap

.PHONY: rust
rust:
	@mkdir -p ./build
	cargo build --target-dir build/

clean:
	rm -rf build
	cargo clean
