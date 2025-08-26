FROM rocker/rstudio:4.5.1

# Copy the whole project folder
COPY . /home/rstudio/ergm

# Install dependency libraries
RUN  . /etc/environment \
 && chmod -R 777 /home/ \
 && apt-get update \
 && apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsqlite3-dev libglpk-dev \
 && R -q -e 'install.packages(c("arsenal", "btergm", "cowplot", "dplyr", "ergm", "ggnetwork", "ggplot2", "igraph", "intergraph", "network", "openxlsx", "plotly", "plyr", "purrr", "R.utils", "RColorBrewer", "readxl", "Rglpk", "sf", "statnet", "tergm", "texreg", "tidygraph", "viridis", "viridisLite", "xts"))' 

### STEP 1 ###
# Run on the terminal:
# docker build -t ergm .

### STEP 2 ###
# Run on the terminal:
# docker run --rm -it -e ROOT=TRUE -e PASSWORD=rstudio -dp 8787:8787 ergm

### STEP 3 ###
# Go to http://localhost:8787/ on your browser. USERID=rstudio, PASSWORD=rstudio
# Please note the default working directory is '/home/rstudio' so in order to execute the scripts you should run in the browser 
#  RStudio console:
# setwd ('~/ergm') 
# first.
# Run the scripts in the browser RStudio console in order using:
# source ("1.DNA_V1.R")
# source ("2.ERGM_periods_hybrid_V1.R")
# source ("3.MTERGM_hybrid_V1.R")
#
# Or open up the scripts in turn and run them interactively

### STEP 4 ####
# Clean and delete containers. Run on the terminal:
# docker ps -aq | xargs docker stop | xargs docker rm

# This will generate a folder called output in your home directory containing figures and tables as well as R images containing the results of the case study  and the tactical simulation.

# Please note that the scripts will require about * hours for completion on a desktop machine.