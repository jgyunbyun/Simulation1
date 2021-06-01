## README.md

### Author

Jaegyun Byun

Institute for Environmental Reesearch
RWTH Aachen University
Worringerweg 1
52074 Aachen
Current contact: jae-gyun.byun@rwth-aachen.de
Last update, January 2021

### Aim

To generate and implement a simulation model in order to realistically depict the stands’ natural dynamics of stem volume development resulting from three different management approaches such as (1) plantation, (2) continuous cover forestry, and (3) reserved forestry. 

### Features

Develop a hybrid model using a wrapper functionHow to simulate:

### How the model works

1. Starts with Autoit wrapper
2. Produces seeds
3. Calculates competition index in Silva
4. Removes competitive seedlings
5. Selects trees which will not die
6. Simulates 1 growth period in Silva
7. Goto 2.

### Dependencies

1. An existing tree growth model: here SILVA forest simulator was selected for using validated tree growth calculation.
2. An automation software: here Autoit
3. R for computing the regeneration and the local forest management

### How to use?

There are three code files that need to be adjusted to your needs and run: autoit, seed, and subset

1. Before starting the Autoit script, you need an initial .slv text file as SILVA input.
2. You need to change the path in two of the R scripts (seed: line 56, subset: line 190) in autoit script.
3. Change the simulation period at line 15 line
4. Run the Autoit script.
