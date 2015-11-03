.PHONY: BNFC
BNFC: Instant.cf ../.cabal-sandbox/bin/happy ../.cabal-sandbox/bin/alex ../.cabal-sandbox/bin/bnfc
	../.cabal-sandbox/bin/bnfc -p BNFC Instant.cf
	../.cabal-sandbox/bin/happy -gca BNFC/ParInstant.y
	../.cabal-sandbox/bin/alex -g BNFC/LexInstant.x
	touch BNFC/*

../.cabal-sandbox/bin/happy: ../Makefile
	$(MAKE) -C .. .cabal-sandbox/bin/happy

../.cabal-sandbox/bin/alex: ../Makefile
	$(MAKE) -C .. .cabal-sandbox/bin/alex

../.cabal-sandbox/bin/bnfc: ../Makefile
	$(MAKE) -C .. .cabal-sandbox/bin/bnfc