ARG R_LIBS="remotes,tidyverse,RcppArmadillo,RcppParallel,devtools,RcppThread,openxlsx,parallel,aricode,readxl,gridExtra"

FROM rocker/rstudio:4.4.1

ARG R_LIBS

# Install required libraries 
RUN R -e "install.packages(unlist(strsplit('${R_LIBS}', ',')))"
COPY ./PAGFLReplication_0.0.1.tar.gz /home/rstudio/PAGFLReplication_0.0.1.tar.gz
RUN R CMD INSTALL /home/rstudio/PAGFLReplication_0.0.1.tar.gz

# Import data, scripts, and files
COPY ./Data /home/rstudio/Data
COPY ./estim_CO2_intensity.rds /home/rstudio/estim_CO2_intensity.rds
COPY ./LICENSE /home/rstudio/LICENSE
COPY ./README.md /home/rstudio/README.md
COPY ./MC_settings.csv /home/rstudio/MC_settings.csv
COPY ./Empirical_Illustration.qmd /home/rstudio/Empirical_Illustration.qmd
COPY ./Helper_Functions.R /home/rstudio/Helper_Functions.R
COPY ./MC_Simulation_Study.qmd /home/rstudio/MC_Simulation_Study.qmd

RUN chown -R rstudio:rstudio /home/rstudio/Data

EXPOSE 8787
CMD ["/init"]