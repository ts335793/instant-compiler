.PHONY: BNFC
BNFC: Instant.cf
	bnfc -p BNFC Instant.cf
	happy -gca BNFC/ParInstant.y
	alex -g BNFC/LexInstant.x
	touch BNFC/*