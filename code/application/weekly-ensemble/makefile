# covidEnsembles: make file for building ensembles each week

# 2 processes to do plots and ensemble build in parallel
MAKEFLAGS += -j2

# Define global variables
# This is a directory that is a local clone of reichlab/covid19-forecast-hub
HUB = ../../../../covid19-forecast-hub/

# data-processed folder within the covid19-forecast-hub repo
PROCESSED = $(HUB)data-processed/

# Define the full make, making plots and
all: incvscum ensemble pull data

# Get the latest forecast files from the hub repository
pull:
	git -C $(HUB) pull origin master

# Update and reinstall covidData package
data:
	cd ../../../../covidData/code/data-processing; make all

# Plot submissions from component models
plots: pull data
	Rscript plot_submissions.R

# Build the ensemble
ensemble: pull data
	Rscript build_ensembles.R

# Compare one week ahead forecasts of incident and cumulative deaths at
# national level
incvscum: pull data
	Rscript inc_vs_cum.R

# TODO: commit, push to github, file pull request
