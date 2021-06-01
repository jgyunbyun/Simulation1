### README.md

How to simulate:

1. Start with Autoit
2. Produce seeds
3. Calculate competition index in Silva
4. Remove competitive seedlings
5. Select trees which will not die
6. Simulation 1 periods in Silva

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

Develop a hybrid model using a wrapper function

### Dependencies

%Existing Tree growth models, here SILVA forest simulator was selected for using validated tree growth calculation.
%Automation software; Autoit
%R for computing the regeneration and the local forest management

### How to use?

There are three code files such as autoit, seed, and subset.

1. Before starting the Autoit script, you need an initial .slv text file as SILVA input.

2. You need to change the path in two of the R scripts (seed: line 56, subset: line 190) in autoit script.

3. Change the simulation period at line 15 line

4. Run the Autoit script.

