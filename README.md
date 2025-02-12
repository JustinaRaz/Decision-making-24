# MSc Cognitive Science - Decision Making
This repository contains the code for the exam project of Decision Making course in Aarhus University.

The below section describes the repository organization.
Note: Some files/scripts have been called "...adjusted". The latter means that the output or script is generated using Bayesian inference [JAGS] with increased number of chains and iterations.

## Repository organization
```
├── Data_preprocessing/                  <- Pre-processing of Wood et al. (2005) data.
|   ├── Data/                            <- Original IGT data.
|   ├── data_preprocessing.Rmd           <- IGT data preprocessing.
|   ├── older_group.csv                  <- IGT data [OLDER].
|   └── younger_group.csv                <- IGT data [YOUNGER].
├── parameter_estimation/                <- Scripts for hORL parameter estimation.
|   ├── PPC/                             <- Posterior predictive checks [PPC].
|   |   ├── plots/                       <- PPC plots [BOTH GROUPS].
|   |   ├── output/                      <- Saved PPC output [BOTH GROUPS].
|   |   ├── ORL_no_theta.txt             <- ORL model.
|   |   ├── older_ppc.R                  <- Run PPC [OLDER].
|   |   ├── younger_ppc.R                <- Run PPC [YOUNGER].
|   |   ├── older_ppc_processing.R       <- To visualize PPC findings [OLDER]
|   |   └── younger_ppc_processing.R     <- To visualize PPC findings [YOUNGER]
|   ├── data/                            <- IGT datasets [FROM PRE-PROCESSING].
|   ├── output_data/                     <- 4 estimation datasets. [2 lower-iteration model, 2 adjusted model].
|   ├── plots/                           <- Parameter estimation convergence plots.
|   ├── hier_ORL_no_theta.txt            <- hORL model.
|   ├── hier_ORL_old.R                   <- Parameter estimation [OLDER DATA]
|   ├── hier_ORL_young.R                 <- Parameter estimation [YOUNGER DATA]
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
├── combined_recovery_plots.R            <- Plotting parameter recovery [BOTH groups in one plot].
├── old_recovery.csv                     <- Combined recovery data [OLDER].
├── young_recovery.csv                   <- Combined recovery data [YOUNGER].
├── hier_ORL_no_theta.txt                <- hORL model specification.                 
├── hier_recovery_old_processing.R       <- Script for recovery plots, N = 63 [older group].
├── hier_recovery_old_processing_unadjusted.R  <- Script for running parameter recovery hORL model, older group, at once.
├── hier_recovery_young_processing.R     <- Script for recovery plots, N = 90 [younger group].
├── old_hier_ORL_recovery.R              <- Parameter recovery [LESS iterations, N = 63].   
├── old_hier_ORL_recovery_adjusted.R     <- Parameter recovery [MORE iterations, N = 63].  
├── plots_old_recovery_adjusted.R        <- Recovery plots, N = 63 [older group].
├── plots_young_recovery_adjusted.R      <- Recovery plots, N = 90 [younger group].
├── recov_plot.R                         <- Plotting script.
└── young_hier_ORL_recovery_adjusted.R   <- Parameter recovery [MORE iterations, N = 90]. 
```
