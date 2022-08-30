-include Makefile.config

GHC  ?= ghc
ODIR ?= build
CC   ?= gcc -O2

all: fcomp

fcomp: $(ODIR)
	$(GHC) --make -O Main.hs -outputdir $(ODIR) -o $(ODIR)/fcomp

$(ODIR):
	mkdir $(ODIR)

test: fcomp
	$(ODIR)/fcomp $(SRC) $(ODIR)/main.c
	@echo
	@$(CC) $(ODIR)/main.c -o $(ODIR)/main
	@echo $(INPUT) | $(ODIR)/main

clean:
	rm -r $(ODIR)

.PHONY: fcomp test clean