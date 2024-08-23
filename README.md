# WannierBandPlot
Plot the bandstructures of wannier90 _hr.dat files using Fortran and gnuplot

*Input files:* 
- First is the master input file.
- Second is the _hr.dat output file from a wannier90 calculation. This needs the filename ’*seedname*_hr.dat'
- Third is the .nnkp file. This needs the filename ’*seedname*.nnkp'
- Fourth is a kpoints file.
  
*Output files:*
- First is *seedname*_band.dat This is the band structure in a format that gnuplot can work with
  - An rgb variable is given to show the orbital projections. Equally spaced hues are chosen for the base colours to give the highest contrast 
- Second is *seedname*_band.gnu. This is a simple gnuplot script to plot *seedname*_band.dat

## Master input file
- This file needs the filename 'INPUT'
1. *seedname* (eg KTaO<sub>3</sub>, SrTiO<sub>3</sub>, BiTeI etc...)
2. The number of orbitals. For example, if we are considering the *p* orbitals of BiTeI, this will be 3
3. The size of the resulting figure, in inches(in) or centimetres(cm)
4. The basis, the current version of wannier90 the hr file in the up, down, up down... basis whereas the old version writes up, up ..., up, down, down, ..., down
5. The fermi level in electron-volts. This is an optional tag. If specified, this value will be subtracted from the energies when the band structure is plotted
6. The y limits (optional) - the range for the y axis
### Example
    seedname   SrTiO3
    norb       3
    figsize    6 4 in
    basis      uudd
    e_fermi    7.753
    yrange     1.00 4.5
*Spaces must be used for separation! Tabs will cause errors!*

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
