# Compiler
FC=mpif90
#FFLAGS=-g -warn
FFLAGS=-g

# Directories
PWD    = $(shell pwd)
OBJDIR = $(PWD)/obj
SRCDIR = $(PWD)/src
LIBDIR = $(PWD)/UTILS/lib
IDIR   = $(PWD)/UTILS/include

## Set ADIOS_DIR here or before doing make
override ADIOS_DIR:=/home/lei/bin/adios-1.5.0
#override ADIOS_DIR:=/sw/redhat6/adios/1.5.0/rhel6_pgi13.4
override ADIOS_INC:=` ${ADIOS_DIR}/bin/adios_config -c -f`
override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`

# Libraries
LIBS = -lsacio -lsac

# Files and folders
TARGET = sac_to_asdf
_OBJ = adios_helpers_definitions.o adios_helpers_writers.o \
			adios_helpers.o asdf_data.o asdf_subs.o \
			seismo_variables.o main_subs.o main.o

# Make all
all: $(OBJDIR) $(TARGET)

OBJ = $(patsubst %,$(OBJDIR)/%,$(_OBJ))

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS) -module $(OBJDIR) $(ADIOS_INC)

$(TARGET) : $(OBJ)
	$(FC) -o $@ $^ $(FFLAGS) -I$(OBJDIR) -L$(LIBDIR) $(LIBS) -I$(IDIR) $(ADIOS_FLIB)

.PHONY: clean

clean:
	rm -f $(OBJDIR)/*.f90 $(OBJDIR)/*.o $(OBJDIR)/*.mod core.* NUMRECORDS OBSD_FILES SYNT_FILES
