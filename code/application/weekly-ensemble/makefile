# covidEnsembles: make file for building ensembles each week

# 2 processes to do plots and ensemble build in parallel
MAKEFLAGS += -j2

# Define global variables
# This is a directory that is a local clone of reichlab/covid19-forecast-hub
HUB = ../../../../covid19-forecast-hub/

# data-processed folder within the covid19-forecast-hub repo
PROCESSED = $(HUB)data-processed/

# Define the full make, making plots and
all: plots ensemble pull

# Get the latest forecast files from the hub repository
pull:
	git -C $(HUB) pull origin master

# Plot submissions from component models
plots: pull
	Rscript plot_submissions.R

# Build the ensemble
ensemble: pull
	Rscript build_ensembles.R

# TODO: commit, push to github, file pull request
