FC = gfortran
FCFLAGS = -ffree-line-length-none
#LDFLAGS -iexamplelib

PROGRAMS = TestBond

all: $(PROGRAMS)

TestBond.o: ForceFieldFunctions.o CoordinateFunctions.o ChemicalSystem.o Conformation.o

TestBond: ForceFieldFunctions.o CoordinateFunctions.o ChemicalSystem.o Conformation.o

Conformation.o: ChemicalSystem.o CoordinateFunctions.o ForceFieldFunctions.o

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod *.MOD
