-include Makefile.config

GHC  ?= ghc
ODIR ?= build

all: fcomp

fcomp: $(ODIR)
	$(GHC) --make -O Main.hs -outputdir $(ODIR) -o $(ODIR)/fcomp

$(ODIR):
	mkdir $(ODIR)

clean:
	rm -r $(ODIR)

.PHONY: fcomp clean