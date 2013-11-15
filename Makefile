# Compiler
FC=mpif90
#FFLAGS=-g -warn
FFLAGS=-g -check all -traceback

# Directories
PWD    = $(shell pwd)
OBJDIR = $(PWD)/obj
SRCDIR = $(PWD)/src
#LIBDIR = $(PWD)/UTILS/lib
#IDIR   = $(PWD)/UTILS/include

## Set ADIOS_DIR here or before doing make
override ADIOS_DIR:=/home/lei/bin/adios-1.5.0
#override ADIOS_DIR:=/sw/redhat6/adios/1.5.0/rhel6_pgi13.4
override ADIOS_INC:=` ${ADIOS_DIR}/bin/adios_config -c -f`
override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`

# Libraries
#LIBS = -lsacio -lsac -lm

# Files and folders
TARGET = combine_seis
_OBJ = var_main.o combine_seis.o main_subs.o main.o

# Make all
all: $(OBJDIR) $(TARGET)

OBJ = $(patsubst %,$(OBJDIR)/%,$(_OBJ))


$(OBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS) -module $(OBJDIR) $(ADIOS_INC)

make_asdf:
	cd src/asdf_util; make

allobj = obj/var_main.o obj/combine_seis.o \
				 obj/main_subs.o obj/main.o \
				 obj/adios_helpers_definitions.o obj/adios_helpers_writers.o \
				 obj/adios_helpers.o obj/asdf_data.o obj/asdf_subs.o

$(TARGET) : make_asdf $(OBJ)
	$(FC) -o $@ $(allobj) $(FFLAGS) -I$(OBJDIR) $(ADIOS_FLIB)

.PHONY: clean

clean:
	rm -f $(OBJDIR)/*.f90 $(OBJDIR)/*.o $(OBJDIR)/*.mod core.* NUMRECORDS OBSD_FILES SYNT_FILES $(TARGET)
