$(BUILD)/Development/Pot/Modules.o : ./Development/Pot/Modules.lhs \
    $(BUILD)/Development/Pot/Packages.hi
	-mkdir -p $(BUILD)/Development/Pot/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filemanip -package filepath -package groom -c $< -o $(BUILD)/Development/Pot/Modules.o \
        -i$(BUILD)/

$(BUILD)/Development/Pot/Packages.o : ./Development/Pot/Packages.lhs
	-mkdir -p $(BUILD)/Development/Pot/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package process -c $< -o $(BUILD)/Development/Pot/Packages.o \
        -i$(BUILD)/

$(BUILD)/Setup.o : ./Setup.hs
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package Cabal -c $< -o $(BUILD)/Setup.o \
        -i$(BUILD)/

$(BUILD)/dist/build/autogen/Paths_package_o_tron.o : ./dist/build/autogen/Paths_package_o_tron.hs
	-mkdir -p $(BUILD)/dist/build/autogen/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/dist/build/autogen/Paths_package_o_tron.o \
        -i$(BUILD)/

$(BUILD)/exe-src/Dump.o : ./exe-src/Dump.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/Dump.o \
        -i$(BUILD)/

$(BUILD)/exe-src/DumpPackageDB.o : ./exe-src/DumpPackageDB.lhs \
    $(BUILD)/Development/Pot/Packages.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/DumpPackageDB.o \
        -i$(BUILD)/

$(BUILD)/exe-src/Makefilerize.o : ./exe-src/Makefilerize.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filepath -c $< -o $(BUILD)/exe-src/Makefilerize.o \
        -i$(BUILD)/

$(BUILD)/exe-src/ShowPackages.o : ./exe-src/ShowPackages.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/exe-src/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/exe-src/ShowPackages.o \
        -i$(BUILD)/

$(BUILD)/Dump.o : exe-src/Dump.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Dump.o \
        -i$(BUILD)/

$(BUILD)/DumpPackageDB.o : exe-src/DumpPackageDB.lhs \
    $(BUILD)/Development/Pot/Packages.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/DumpPackageDB.o \
        -i$(BUILD)/

$(BUILD)/Makefilerize.o : exe-src/Makefilerize.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package filepath -c $< -o $(BUILD)/Makefilerize.o \
        -i$(BUILD)/

$(BUILD)/ShowPackages.o : exe-src/ShowPackages.lhs \
    $(BUILD)/Development/Pot/Modules.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/ShowPackages.o \
        -i$(BUILD)/
$(BUILD)/Makefilerize : $(BUILD)/Makefilerize.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(MAKEFILERIZE_EXTRA) \
    -o $(BUILD)/Makefilerize \
    $(BUILD)/Makefilerize.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process

$(BUILD)/Dump : $(BUILD)/Dump.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(DUMP_EXTRA) \
    -o $(BUILD)/Dump \
    $(BUILD)/Dump.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process

$(BUILD)/DumpPackageDB : $(BUILD)/DumpPackageDB.o \
    $(BUILD)/Development/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(DUMPPACKAGEDB_EXTRA) \
    -o $(BUILD)/DumpPackageDB \
    $(BUILD)/DumpPackageDB.o \
    $(BUILD)/Development/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package groom \
    -package process

$(BUILD)/ShowPackages : $(BUILD)/ShowPackages.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(SHOWPACKAGES_EXTRA) \
    -o $(BUILD)/ShowPackages \
    $(BUILD)/ShowPackages.o \
    $(BUILD)/Development/Pot/Modules.o \
    $(BUILD)/Development/Pot/Packages.o \
    -hide-all-packages \
    -package base \
    -package filemanip \
    -package filepath \
    -package groom \
    -package process


%.hi : %.o
	@:
