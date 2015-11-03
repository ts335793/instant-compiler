.PHONY: all
all: .cabal-sandbox/bin/InstantCompiler

.PHONY: run
run: .cabal-sandbox/bin/InstantCompiler
	.cabal-sandbox/bin/InstantCompiler

.cabal-sandbox/bin/InstantCompiler: src/Main.hs src/JVM.hs src/LLVM.hs src/BNFC/AbsInstant.hs src/BNFC/ErrM.hs src/BNFC/LexInstant.hs src/BNFC/ParInstant.hs src/BNFC/PrintInstant.hs src/BNFC/SkelInstant.hs src/BNFC/TestInstant.hs
	cabal install -j

src/BNFC/AbsInstant.hs src/BNFC/ErrM.hs src/BNFC/LexInstant.hs src/BNFC/ParInstant.hs src/BNFC/PrintInstant.hs src/BNFC/SkelInstant.hs src/BNFC/TestInstant.hs: src/Makefile
	$(MAKE) -C src BNFC

.cabal-sandbox/bin/happy: .cabal-sandbox/sandbox
	cabal install happy

.cabal-sandbox/bin/alex: .cabal-sandbox/sandbox
	cabal install alex

.cabal-sandbox/bin/bnfc: .cabal-sandbox/sandbox
	cabal install bnfc

.cabal-sandbox/sandbox:
	cabal sandbox init
	cabal update
	touch .cabal-sandbox/sandbox

.PHONY: clean
clean:
	-rm .cabal-sandbox/bin/InstantCompiler
	-rm -r src/BNFC