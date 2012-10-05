$(BUILD)/Distribution/Pot/Modules.o : ./Distribution/Pot/Modules.lhs \
    $(BUILD)/Distribution/Pot/Packages.hi
	-mkdir -p $(BUILD)/Distribution/Pot/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filemanip -package filepath -package groom -c $< -o $(BUILD)/Distribution/Pot/Modules.o \
        -i$(BUILD)/

$(BUILD)/Distribution/Pot/Packages.o : ./Distribution/Pot/Packages.lhs
	-mkdir -p $(BUILD)/Distribution/Pot/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package process -c $< -o $(BUILD)/Distribution/Pot/Packages.o \
        -i$(BUILD)/

$(BUILD)/Setup.o : ./Setup.hs
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package Cabal -c $< -o $(BUILD)/Setup.o \
        -i$(BUILD)/

$(BUILD)/exe-src/CabalLint.o : ./exe-src/CabalLint.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package Cabal -package base -package filepath -package packdeps -c $< -o $(BUILD)/exe-src/CabalLint.o \
        -i$(BUILD)/

$(BUILD)/exe-src/Dump.o : ./exe-src/Dump.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/Dump.o \
        -i$(BUILD)/

$(BUILD)/exe-src/DumpPackageDB.o : ./exe-src/DumpPackageDB.lhs \
    $(BUILD)/Distribution/Pot/Packages.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/DumpPackageDB.o \
        -i$(BUILD)/

$(BUILD)/exe-src/Makefilerize.o : ./exe-src/Makefilerize.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filepath -c $< -o $(BUILD)/exe-src/Makefilerize.o \
        -i$(BUILD)/

$(BUILD)/exe-src/ShowPackages.o : ./exe-src/ShowPackages.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/ShowPackages.o \
        -i$(BUILD)/

$(BUILD)/CabalLint.o : exe-src/CabalLint.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package Cabal -package base -package filepath -package packdeps -c $< -o $(BUILD)/CabalLint.o \
        -i$(BUILD)/

$(BUILD)/Dump.o : exe-src/Dump.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Dump.o \
        -i$(BUILD)/

$(BUILD)/DumpPackageDB.o : exe-src/DumpPackageDB.lhs \
    $(BUILD)/Distribution/Pot/Packages.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/DumpPackageDB.o \
        -i$(BUILD)/

$(BUILD)/Makefilerize.o : exe-src/Makefilerize.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filepath -c $< -o $(BUILD)/Makefilerize.o \
        -i$(BUILD)/

$(BUILD)/ShowPackages.o : exe-src/ShowPackages.lhs \
    $(BUILD)/Distribution/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/ShowPackages.o \
        -i$(BUILD)/
$(BUILD)/Makefilerize : $(BUILD)/Makefilerize.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(MAKEFILERIZE_EXTRA) \
    -o $(BUILD)/Makefilerize \
    $(BUILD)/Makefilerize.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process

$(BUILD)/Dump : $(BUILD)/Dump.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(DUMP_EXTRA) \
    -o $(BUILD)/Dump \
    $(BUILD)/Dump.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process

$(BUILD)/DumpPackageDB : $(BUILD)/DumpPackageDB.o \
    $(BUILD)/Distribution/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(DUMPPACKAGEDB_EXTRA) \
    -o $(BUILD)/DumpPackageDB \
    $(BUILD)/DumpPackageDB.o \
    $(BUILD)/Distribution/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package groom \
    -package process

$(BUILD)/ShowPackages : $(BUILD)/ShowPackages.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(SHOWPACKAGES_EXTRA) \
    -o $(BUILD)/ShowPackages \
    $(BUILD)/ShowPackages.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process

$(BUILD)/CabalLint : $(BUILD)/CabalLint.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(CABALLINT_EXTRA) \
    -o $(BUILD)/CabalLint \
    $(BUILD)/CabalLint.o \
    $(BUILD)/Distribution/Pot/Modules.o \
    $(BUILD)/Distribution/Pot/Packages.o \
    -hide-all-packages \
    -package Cabal \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package packdeps \
    -package process


%.hi : %.o
	@:
