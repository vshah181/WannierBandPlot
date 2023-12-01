OBJFILES = constants.o file_parsing.o fourier_transform.o kpath_maker.o main.o
PROGRAM = wannier_plot
FTN = gfortran
FTNFLAGS = -llapack -lblas -O4

all: $(PROGRAM)

$(PROGRAM): $(OBJFILES)
	$(FTN) $(FTNFLAGS) -o $(PROGRAM) $(OBJFILES)

%.o: %.f90
	$(FTN) $(FTNFLAGS) -c $< -o $@

clean:
	rm $(OBJFILES) $(PROGRAM)
