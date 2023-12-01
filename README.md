# WannierBandPlot
Plot the bandstructures of wannier90 _hr.dat files using Fortran and gnuplot
Takes two input files: 
- First is the _hr.dat output file from a wannier 90 calculation.
- Second is a kpoints file.
## kpoints file
- This file needs the filename 'kpoints'
1. number of k points per path
2. number of high symmetry points
3. high symmetry point 1 (in fractional coordinates) and its symbol
4. high symmetry point 2 (in fractional coordinates) and its symbol
5. etc...
### Example
    60
    6
    0.00 0.00 0.00 G
    0.00 0.50 0.00 X
    0.50 0.50 0.00 M
    0.00 0.00 0.00 G
    0.50 0.50 0.50 R
    0.00 0.50 0.00 X
In this example, there are 300 kpoints in total and we go along the $\Gamma$ - X - M - $\Gamma$ - R - X direction
