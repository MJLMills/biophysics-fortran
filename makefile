FC = gfortran
FCFLAGS = -ffree-line-length-none -fmax-errors=0 -std=f95 -Wall
#LDFLAGS -iexamplelib

PROGRAMS = TestBond

all: $(PROGRAMS)

TestBond.o: ChemicalSystem.o Conformation.o Dynamics.o

TestBond: ChemicalSystem.o Conformation.o CoordinateFunctions.o ForceFieldFunctions.o VectorMath.o Dynamics.o

Conformation.o: ChemicalSystem.o CoordinateFunctions.o ForceFieldFunctions.o

CoordinateFunctions.o: VectorMath.o

Dynamics.o: Conformation.o

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod *.MOD
