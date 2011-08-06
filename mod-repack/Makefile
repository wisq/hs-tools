GHC_OPTS=-O2 -Wall

all: mod-repack

clean:
	rm -f mod-repack mod-repack.hi mod-repack.o

mod-repack: mod-repack.hs
	ghc $(GHC_OPTS) --make $<
