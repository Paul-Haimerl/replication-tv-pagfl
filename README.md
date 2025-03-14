# Estimation of Latent Group Structures in Time-Varying Panel Data Models

This repository contains code to replicate the simulation study and empirical illustration in **Haimerl, P., Smeekes, S., & Wilms, I. (2025). Estimation of latent group structures in time-varying panel data models.**

Please feel free to reach out in case of any questions or bugs.

## Contents

3. **Data folder**: Data on GDP and CO_2 emissions employed in the empirical illustration. Both datasets are available at [data.worldbank.org/indicator/NY.GDP.MKTP.CD](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD) and [globalcarbonbudgetdata.org](https://globalcarbonbudgetdata.org) (accessed November 14, 2024). Additionally, the folder contains a map for country names between the two datasets.
1. **Empirical_Illustration.qmd**: Notebook to replicate the empirical illustration in Section 5 and the associated supplements in the Appendix.
2. **estim_CO2_intensity.rds**: The estimation output of the time-varying grouped panel data model presented in Section 5.
4. **Helper_Functions.R**: A script of auxiliary functions for the simulation study and the empirical illustration.
5. **MC_settings.csv**: A table of estimation settings for the simulation study.
2. **MC_Simulation_Study.qmd**: Notebook to replicate the Monte Carlo simulation study in Section 4 and the additional material in the Appendix.
6. **PAGFLReplication_0.0.1.tar.gz**: R-package based on v1.1.3 of [PAGFL](https://github.com/Paul-Haimerl/PAGFL). `tv_pagfl()` of `PAGFLReplication` also returns the *PSE* coefficient estimates in addition to the post-Lasso, which are required for the simulation study. Furthermore, it includes the function `sim_tv_paper()` to conveniently replicate the simulated DGPs employed in Section 5.

> [!NOTE]
> The package `PAGFLReplication` and its dependencies (`RcppParallel`, `RcppThread`, `RcppArmadillo`, `ggplot2`) must be installed.

## Docker

We also provide a Docker image with all packages pre-installed and data uploaded for easy replicability. The image runs R 4.4.1, which was used to produce the results in the paper.

## PAGFL

The official companion package to the paper `PAGFL` is available [here](https://github.com/Paul-Haimerl/PAGFL).
