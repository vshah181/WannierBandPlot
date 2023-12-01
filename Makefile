OBJFILES = constants.o file_parsing.o fourier_transform.o kpath_maker.o colour_calculator.o main.o
PROGRAM = wannier_plot
FTN = gfortran
FTNFLAGS = -llapack -lblas -O4

all: $(PROGRAM)

$(PROGRAM): $(OBJFILES)
	$(FTN) -o $(PROGRAM) $(OBJFILES) $(FTNFLAGS)

%.o: %.f90
	$(FTN) -c $< -o $@ $(FTNFLAGS) 

clean:
	rm $(OBJFILES) $(PROGRAM)
