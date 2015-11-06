.PHONY: all
all: $(HOME)/.cabal/bin/InstantCompiler

.PHONY: run
run: $(HOME)/.cabal/bin/InstantCompiler
	$(HOME)/.cabal/bin/InstantCompiler

$(HOME)/.cabal/bin/InstantCompiler: src/Main.hs src/JVM.hs src/LLVM.hs src/BNFC/AbsInstant.hs src/BNFC/ErrM.hs src/BNFC/LexInstant.hs src/BNFC/ParInstant.hs src/BNFC/PrintInstant.hs src/BNFC/SkelInstant.hs src/BNFC/TestInstant.hs
	cabal install -j

src/BNFC/AbsInstant.hs src/BNFC/ErrM.hs src/BNFC/LexInstant.hs src/BNFC/ParInstant.hs src/BNFC/PrintInstant.hs src/BNFC/SkelInstant.hs src/BNFC/TestInstant.hs: src/Makefile
	$(MAKE) -C src BNFC

.PHONY: clean
clean:
	-rm $(HOME)/.cabal/bin/InstantCompiler
	-rm -r src/BNFC
	-rm tests/*.class
	-rm tests/*.j
	-rm tests/*.ll
	-rm tests/*.bc