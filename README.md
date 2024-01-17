# Evaluating performance of the Urnings algorithm for ability tracking: Speed of convergence and capability to follow change
## Codebase to replicate the analyses of the paper

Team: 
 - **Bence Gergely** (Eötvös Lóránd University, Károli University of the Reformed Church, Budapest, Hungary)
 - Matthieu Brinkhuis (Utrecht University, Utrecht, The Netherlands)
 - Szabolcs Takács (Károli University of the Reformed Church, Budapest, Hungary)
 - **Maria Bolsinova** (Tilburg University, Tilburg, The Netherlands)

### Aim

This study uses a large-scale simulation analysis to investigate how different parameters and system features influence the convergence and ability-tracking capability of the Urnings algorithm. 
The goal is to provide recommendations for building and setting up adaptive learning systems for ideal convergence and ability tracking. 

### Structure of the code
At the main level, one can find the functions for writing simulations using the Urnings algorithm. These functions are separated into different files. Which are
 
 - urnings_handler.R
 - urnings_function.R
 - rasch.R
 - item_selection.R
 - paired_update.R

The _paired_update.R, item_selection.R, rasch.R_ files contain functional components for the different parts of the Urnings algorithm. The _rasch.R_ function contains the necessary functions
to generate expected responses, to simulate true outcomes, to calculate Urnings's updating step, and to apply the Metropolis corrections given the formulation of the algorithm.
The _item_selection.R_ contains the functions that calculate the selection probability between a student and an item given their ability estimates and the adaptive item selection
functionality based on the selection probabilities. Last, the _paired_update.R_ contains the function to operate the item-paired-update algorithm. 

The _urnings_function.R_ is a function following a factory design, meaning that it takes the aforementioned functional components as arguments. This allows for setting up the simulated system
with different features. The _urnings_handler.R_ provides a user-friendly interface for the _urnings_function.R_

### Structure of the analyses
The simulations are located in separate folders. For simulations with stable latent ability go to _simulation1_, for simulations with changing latent ability go to _simulation2_. The folder
_post_hoc_ contains analyses which were conducted after the results of the first two simulations were known. These are extra analyses to better understand the results and to create boundary
conditions. The folder _large_system_simulation_ is an auxiliary analysis to check whether the simulation design has any bias due to the smaller-sized systems. 

The structure of the analysis is the following. 1) Each analysis is divided into multiple scripts to allow for parallel execution (e.g. sim1_urning1_central.R and sim2_urnings2_central.R are
part of the first simulation but they are running a slightly different variant of the Urnings algorithm). All analysis scripts produce a _.rds_ file containing the parameters of the given
system and the ability estimates for every iteration of the simulation. For every system we created a baseline system based on the limiting distribution of the chain of ability estimates,
these baseline results were generated with scripts with names indicating _baseline_ (e.g. _sim1_baseline.R_). Last, the plots and different outcome measures were calculated based on the scripts
with a name indicating _results_ (e.g. _sim2_linear_results.R_). 


### Availability of the results
All resulting _.rds_ files are uploaded to Figshare and can be located using this link: 
All plots that we included in the manuscript can be found in the _figures_ folders under the main.
The supplementary material containing additional results we refer to in the manuscript, the pseudo-algorithmic description of the paired update procedure and the derivations of the limiting
distribution of the modified Urnings algorithm can be found in Figshare using this link: 

### Licensing
All material concerning this research falls under the XY licence. Replication of the results and further analysis is warmly welcomed! The code and the results can be used and modified, if 
and only if they are properly cited!

### Closing remarks
This research was partly funded by the Új Nemzeti Kiválóság Program (ÚNKP) for doctorate students. 

