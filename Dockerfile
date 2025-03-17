FROM rocker/rstudio:4.4.1

# Import data, scripts, and files
COPY . /home/rstudio/

# Install required libraries (versions kept fixed)
ENV CRAN_URL="https://cloud.r-project.org"
RUN R -e "install.packages('remotes', repos='$CRAN_URL')"

# Install specific package versions in one step
RUN R -e "remotes::install_version('tidyverse', version = '2.0.0', repos = '$CRAN_URL'); \
          remotes::install_version('RcppArmadillo', version = '14.2.3-1', repos = '$CRAN_URL'); \
          remotes::install_version('RcppParallel', version = '5.1.10', repos = '$CRAN_URL'); \
          remotes::install_version('devtools', version = '2.4.5', repos = '$CRAN_URL'); \
          remotes::install_version('RcppThread', version = '2.2.0', repos = '$CRAN_URL'); \
          remotes::install_version('openxlsx', version = '4.2.7', repos = '$CRAN_URL'); \
          remotes::install_version('aricode', version = '1.0.3', repos = '$CRAN_URL'); \
          remotes::install_version('readxl', version = '1.4.3', repos = '$CRAN_URL'); \
          remotes::install_version('gridExtra', version = '2.3', repos = '$CRAN_URL')"

RUN R CMD INSTALL /home/rstudio/PAGFLReplication_0.0.1.tar.gz

RUN chown -R rstudio:rstudio /home/rstudio/

CMD ["/init"]