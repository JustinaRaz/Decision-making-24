# MSc Cognitive Science - Decision Making
This repository contains the code for the exam project of Decision Making course in Aarhus University.

## Repository organization
```
├── Data_preprocessing/                  <- Contains all scripts for preprocessing of Wood et al. (2005) data.
|   ├── Data/                            <- Original data from the source.
|   ├── data_preprocessing.Rmd           <- Script for original datas' preprocessing for ORL model.
|   ├── older_group.csv                  <- The output data - IGT data for older participants.
|   └── younger_group.csv                <- The output data - IGT data for younger participants.
├── parameter_estimation/                <- Scripts for hORL parameter estimation.
|   ├── data/                            <- Two output datasets (from data_preprocessing).
|   ├── output_data/                     <- Contains 4 estimation datasets: 2 from lower-iteration model, 2 from adjusted model.
|   ├── plots/                           <- Parameter estimation convergence plots.
|   ├── hier_ORL_no_theta.txt            <- hORL model specification.
|   ├── hier_ORL_old.R                   <- hORL parameter estimation given the older IGT data.
|   ├── hier_ORL_young.R                 <- hORL parameter estimation given the younger IGT data.
|   └── traceplots.R                     <- Script for trace plots of both group estimations + combined density plot.
├── parameter_recovery/                  <- Scripts for hORL parameter recovery (simulated data).
|   ├── JAGS_data/                       <- Contains data from Bayesian inference with JAGS.
|   ├── plots_recovery/                  <- Parameter recovery plots for un-adjusted and adjusted models.
|   ├── traceplots/                      <- Convergence diagnostics plots for un-adjusted and adjusted models.
|       ├── traceplots_old.R             <- Scripts for convergence diagnostics plots for un-adjusted (lower-iteration) model.
|       ├── traceplots_young.R           <- Scripts for convergence diagnostics plots for un-adjusted (lower-iteration) model.
|       ├── traceplots_young_adjusted.R  <- Scripts for convergence diagnostics plots for adjusted (higher-iteration) model.
|       └── traceplots_old_adjusted.R    <- Scripts for convergence diagnostics plots for adjusted (higher-iteration) model.
├── ORL_sim.R                               
├── hier_ORL_no_theta.txt                <- hORL model specification.                 
├── hier_recovery_old_processing.R       <- Script for recovery plots, N = 63 [older group].
├── hier_recovery_old_processing_unadjusted.R  <- Script for running parameter recovery hORL model, older group, at once.
├── hier_recovery_young_processing.R     <- Script for recovery plots, N = 90 [younger group].
├── old_hier_ORL_recovery.R              <- Script for running parameter recovery hORL model (less iterations).   
├── old_hier_ORL_recovery_adjusted.R     <- Script for running parameter recovery hORL model (more iterations).
├── plots_old_recovery_adjusted.R        <- Recovery plots, N = 63 [older group].
├── plots_young_recovery_adjusted.R      <- Recovery plots, N = 90 [younger group].
├── recov_plot.R                         <- Plotting script.
└── young_hier_ORL_recovery_adjusted.R   <- Script for running parameter recovery hORL model (more iterations).
```
