-include Makefile.config

CC     ?= gcc
CFLAGS ?= -Wall -O2 -fno-strict-aliasing

TMP_DIR ?= build

all:
	@cabal build fcomp

$(TMP_DIR):
	mkdir $(TMP_DIR)

test: $(TMP_DIR)
	@cabal run fcomp -- $(SRC) $(TMP_DIR)/main.c
	@echo
	@$(CC) $(CFLAGS) $(TMP_DIR)/main.c -o $(TMP_DIR)/main
	@echo $(INPUT) | $(TMP_DIR)/main

clean:
	cabal clean
	rm -rf $(TMP_DIR)

.PHONY: test clean