########################################################################
####################### Makefile Template ##############################
########################################################################

# Compiler settings - Can be customized.
CC = gfortran
CPP = gfortran -cpp
CXXFLAGS = -g -fallow-argument-mismatch -o3 -fcheck=all -fbounds-check -fbacktrace -Wall -Wextra \
           -fcheck=pointer -fcheck=mem -fimplicit-none -fopenmp -ffpe-summary='none'
LDFLAGS =  -ffat-lto-objects -flto=auto -lm -ffpe-summary='none'

# Makefile settings - Can be customized.
APPNAME = SHUII
EXT = .f90
SRCDIR = src
OBJDIR = obj
MODDIR = mod
############## Do not change anything from here downwards! #############
SRC = $(SRCDIR)/modeltype.f90 $(SRCDIR)/modeltype_sed.f90 $(SRCDIR)/modeltype_ant.f90 $(SRCDIR)/modelvar.f90 $(SRCDIR)/modelvar_sed.f90 \
			$(SRCDIR)/modelvar_ant.f90 $(SRCDIR)/general.f90 $(SRCDIR)/datawrite.f90 $(SRCDIR)/modelpar.f90 $(SRCDIR)/modelpar_sed.f90 $(SRCDIR)/datetime.f90 \
      $(SRCDIR)/modelpar_ant.f90 $(SRCDIR)/initialize.f90 $(SRCDIR)/initialize_ant.f90 $(SRCDIR)/initialize_sed.f90 \
			$(SRCDIR)/initialize_ant.f90 $(SRCDIR)/dataget.f90 $(SRCDIR)/dataget_ant.f90 $(SRCDIR)/aboveground.f90 $(SRCDIR)/gw_ant.f90 $(SRCDIR)/groundwater.f90 \
			$(SRCDIR)/landwater.f90 $(SRCDIR)/reach_ant.f90 $(SRCDIR)/reach_sed.f90 $(SRCDIR)/surfacewater.f90 $(SRCDIR)/erosion.f90 $(SRCDIR)/soil_ant.f90 \
			$(SRCDIR)/run.f90  $(SRCDIR)/main.f90
			
# SRC = $(wildcard $(SRCDIR)/*$(EXT))
OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)
DEP = $(OBJ:$(OBJDIR)/%.o=%.d)

# UNIX-based OS variables & settings
RM = rm
DELOBJ = $(OBJ)
# Windows OS variables & settings
DEL = del
EXE = .exe
WDELOBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)\\%.o)

########################################################################
####################### Targets beginning here #########################
########################################################################

all: $(APPNAME)

# Builds the app
$(APPNAME): $(OBJ)
	$(CC) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

# Creates the dependecy rules
%.d: $(SRCDIR)/%$(EXT)
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:%.d=$(OBJDIR)/%.o) >$@

# Includes all .h files
-include $(DEP)

# Building rule for .o files and its .c/.cpp in combination with all .h
$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT)
	$(CC) $(CXXFLAGS) -o $@ -c $<

################### Cleaning rules for Unix-based OS ###################
# Cleans complete project
.PHONY: clean
clean:
	$(RM) $(DELOBJ) $(DEP) $(APPNAME) 

# Cleans only all files with the extension .d
.PHONY: cleandep
cleandep:
	$(RM) $(DEP)

#################### Cleaning rules for Windows OS #####################
# Cleans complete project
.PHONY: cleanw
cleanw:
	$(DEL) /Q $(OBJDIR)\*.o $(MODDIR)\*.mod $(DEP) $(APPNAME)$(EXE)

# Cleans only all files with the extension .d
.PHONY: cleandepw
cleandepw:
	$(DEL) $(DEP)