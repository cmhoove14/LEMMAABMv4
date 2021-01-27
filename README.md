# LEMMAABMv4  

## Overview  
`LEMMAABMv4` is an agent-based model of COVID-19 transmission dynamics, currently parameterized to represent the city and county of San Francisco. Future development may focus on generalizing data importation and processing for use in other settings. Transmission proceeds through a modified Susceptible-Exposed-Infected-Recovered (SEIR) model that allows for inclusion of pre- and asymptomatic states ($I_p$ and $I_a$ , respectively) and age-dependent variability in symptom severity ($I_m$, mildly symptomatic, and $I_{mh}$ to  $I_h$ for symptomatic pre-hospitalization and hospitalized, respectively) and mortality. Agent movement between census tracts is informed by [Safegraph mobility metrics]() derived from anonymized mobile phone panels. There are no explicit network connections, rather all agents are members of households, workplaces, and communities which they attend depending on circumstances of the simulation and transmission occurs according to a location-specific FOI for each location at each time step.  

## User Guide  
### Installation  
`devtools::install_github("cmhoove14/LEMMAABMv4")`  

Installation will load all scripts necessary to set up and simulate the model, but the `data` folder also contains pre-processed data inputs that will allow users to simulate the model right out of the box. Forking the repository may therefore be the best route to quick and easy setup and simulation.

### Simulation  
The main simulation function is `covid_abm_v4` which takes inputs:  
* `bta_base` - baseline transmission probability  
* `bta_hh` - multiplier on household transmission probability  
* `bta_work` - multiplier on workplace transmission probability  
* `bta_sip_red` - multiplier on transmission probability following onset of first shelter in place to account for microbehaviors affecting transmission that are not explicitly included in the model  
* `data_inputs` - all data inputs including synthetic agents, safegraph movement and stay at home metrics, etc. see documentation  
* `input_pars` - list of lists with additional model parameters and setup  
* `vax_phases` - list of lists delineating vaccine phases  
* `visitors` - logical of whether to model visitors into target population (requires visitors setup in `data_inputs`)  
* `testing` - logical of whether to model testing  
* `adaptive` - logical of whether to model adaptive testing  
* `vaccination` - logical of whether to model vaccination  
* `verbose` - logical of whether to return progress summaries at each time step  
* `store_extra` - logical of whether to store extra output files from simulation  

The `bash` folder also contains scripts to run in parallel via bash scripts in which model inputs are passed in shell  