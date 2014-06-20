FC = gfortran
FCFLAGS = -ffree-line-length-none -fmax-errors=0 -std=f95 -Wall
#LDFLAGS -iexamplelib

PROGRAMS = TestBond

all: $(PROGRAMS)

TestBond.o: ChemicalSystem.o Conformation.o

TestBond: ChemicalSystem.o Conformation.o CoordinateFunctions.o ForceFieldFunctions.o

Conformation.o: ChemicalSystem.o CoordinateFunctions.o ForceFieldFunctions.o

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod *.MOD
