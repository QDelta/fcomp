-include Makefile.config

ODIR=build

all: fcomp

fcomp: $(ODIR)
	ghc --make -O2 Main.hs -outputdir $(ODIR) -o $(ODIR)/fcomp

$(ODIR):
	mkdir $(ODIR)

clean:
	rm -r $(ODIR)

.PHONY: fcomp clean
