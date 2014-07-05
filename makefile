FC = gfortran
FCFLAGS = -ffree-line-length-none -fmax-errors=0 -fopenmp -std=f95 -Wall
#LDFLAGS -iexamplelib

PROGRAMS = TestBond

all: $(PROGRAMS)

TestBond.o: ChemicalSystem.o Conformation.o Dynamics.o OMPTools.o

TestBond: ChemicalSystem.o Conformation.o CoordinateFunctions.o ForceFieldFunctions.o VectorMath.o Dynamics.o OMPTools.o Toolbox.o

Conformation.o: ChemicalSystem.o CoordinateFunctions.o ForceFieldFunctions.o

CoordinateFunctions.o: VectorMath.o Toolbox.o

Dynamics.o: Conformation.o

OMPTools.o: Toolbox.o

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod *.MOD
