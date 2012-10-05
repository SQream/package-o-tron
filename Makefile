
# Example makefile which can compile the source for this project
# also look in the autorules.mk to see the automatically generated
# support. You can regenerate the autorules.mk using the autorules
# rule

# the source directories where the haskell source roots are (paths
# relative to this Makefile)
SRC_DIRS = .

# the names of exe files to compile (these need to be the full paths
# from the Makefile). You have to use ./ if it is in the root folder
EXE_FILES = ./Makefilerize ./Dump ./DumpPackageDB ./ShowPackages

# folder to put build and exe files in
BUILD=build/

# the command and options used to compile .hs/.lhs to .o
HC              = ghc
HC_BASIC_OPTS   = -Wall

space :=
space +=
comma := ,
HC_INCLUDE_DIRS = -i$(subst $(space),:,$(SRC_DIRS))

HC_OPTS = $(HC_BASIC_OPTS) $(HC_INCLUDE_DIRS)

# the command and options used to link .o files to an executable
# usually the same as the compile commands
# you could put options which are needed to link all the exes
# here
# there is also a variable per exe to add link specific commands
# for that exe (e.g. some of your exes need to link to a .so
# and some don't)
HL = $(HC)
HL_OPTS = $(HC_OPTS)


# create an all rule which builds all the exes
# (the exes are all created directly in the $(BUILD) folder
EXE_FILES_TARGETS = $(addprefix $(BUILD), $(notdir $(EXE_FILES)))
all : $(EXE_FILES_TARGETS)

# include the autogenerated rules
-include autorules.mk

# regenerate the dependency and rules for exe compiles
# use cabal configure && cabal build to make the Makefilerize exe
.PHONY : autorules
autorules :
	Makefilerize $(SRC_DIRS) EXES $(EXE_FILES) > \
	autorules.mk

.PHONY : clean
clean :
	-rm -Rf dist/
	-rm -Rf build/
