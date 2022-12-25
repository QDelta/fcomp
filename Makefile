-include Makefile.config

CC     ?= gcc
CFLAGS ?= -O2

TMP_DIR ?= build

$(TMP_DIR):
	mkdir $(TMP_DIR)

test: $(TMP_DIR)
	cabal run fcomp -- $(SRC) $(TMP_DIR)/main.c
	@echo
	@$(CC) $(CFLAGS) $(TMP_DIR)/main.c -o $(TMP_DIR)/main
	@echo $(INPUT) | $(TMP_DIR)/main

clean:
	rm -rf $(TMP_DIR)

.PHONY: test clean