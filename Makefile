OUTPUT=build

Main: 
	ghc --make -O2 Main.hs -outputdir ${OUTPUT}

test: Main

.PHONY: Main test
