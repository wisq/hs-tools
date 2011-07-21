GHC_OPTS=-O2 -Wall

all: main

clean:
	rm -f main main.hi main.o

main: main.hs
	ghc $(GHC_OPTS) --make $<
