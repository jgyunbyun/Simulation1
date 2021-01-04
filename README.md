### README.md

update how to simulate 

1. start with Autoit
2. produce seeds
3. calculating competition index in Silva
4. remove competitive seedlings
5. Select trees which will not die
6. simulation 1 periods in Silva

### README.md

### Author

Jaegyun Byun

Institute for Environmental Reesearch
RWTH Aachen University
Worringerweg 1
52074 Aachen
Current contact: jaegyun.byun@bio5.rwth-aachen.de
Last update, January 2021




### Aim

To generate and implement a simulation model in order to realistically depict the stands’ natural dynamics of stem volume development resulting from these different management approaches such as (1) plantation, (2) continuous cover forestry, and (3) reserved forestry. 

### Features
Develop a hybrid model using Wrapper function


### Dependencies
%Existing Tree growth models, here SILVA forest simulator was selected for using validated tree growth calculation.
%Automation software; Autoit
%R for computing the regeneration and the local forest management




### How to use?
There is three codes such as autoit, seed, subset.

1. Before starting autoit script, you need initial .slv text file as SILVA input.

2. Need to change the path for two R script(seed; line 56 & subset; line 190) in autoit script.

3. Please change the simulation period at 15 line.

4. Run autoit script.

