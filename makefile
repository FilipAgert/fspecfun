# Paths
DSRC = src
DOBJ = build
DEXE = app
DMOD = mod
EXEN = fspec
all: $(DEXE)/$(EXEN)
# Flags
LIBS = 
FLAGS = -O3 -I$(DOBJ) -I$(DMOD) -fcheck=all -fbacktrace -g -ffree-line-length-none -fimplicit-none
CC = gfortran $(FLAGS) -J$(DMOD) $(LIBS) -c
CCL = gfortran -o

# Objects
OBJECTS = $(DOBJ)/fspec.o $(DOBJ)/fspec_gamma.o
MAIN_OBJ = $(DOBJ)/main.o

VPATH = $(DSRC):$(DTEST):$(DSRC)/$(DSH)

$(DOBJ)/main.o: $(DSRC)/main.f90 $(DOBJ)/fspec.o 
$(DOBJ)/fspec.o: $(DSRC)/fspec.f90 

# Default target



$(DOBJ)/%.o: %.f90 | $(DOBJ) $(DMOD)
	$(CC) $< -o $@

# Ensure required directories exist
$(DOBJ) $(DEXE) $(DMOD):
	mkdir -p $@

# Targets
$(DEXE)/$(EXEN): $(MAIN_OBJ) $(OBJECTS) | $(DEXE)
	$(CCL) $@ $(MAIN_OBJ) $(OBJECTS) $(LIBS)

main: $(DEXE)/$(EXEN)

run: $(DEXE)/$(EXEN)
	$(DEXE)/$(EXEN) $(ARG1) $(ARG2)



clean:
	rm -rf $(DOBJ)/*.o $(DEXE)/* $(DMOD)/*.mod *.dat 

.PHONY: all clean run main
