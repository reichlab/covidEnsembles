# covidEnsembles: make file for building ensembles each week

# 2 processes to do plots and ensemble build in parallel
MAKEFLAGS += -j2

# Define global variables
# This is a directory that is a local clone of reichlab/covid19-forecast-hub
HUB = ../../../../covid19-forecast-hub/

# data-processed folder within the covid19-forecast-hub repo
PROCESSED = $(HUB)data-processed/

# current date, for git tags
TODAY_DATE = $(shell date +'%Y-%m-%d')

# Define the full make, making plots and
all: comparative_plots incvscum ensemble pull data covidEnsembles

# Switch to master covidEnsembles branch, pull, and reinstall
covidEnsembles:
	git switch master
	git pull
	cd ../../../../
	R CMD INSTALL covidEnsembles


# Get the latest forecast files from the hub repository
pull:
	git -C $(HUB) fetch --all --tags
	git -C $(HUB) checkout tags/$(TODAY_DATE)-COVIDhub-ensemble

# Update and reinstall covidData package
data:
	cd ../../../../covidData/code/data-processing
	git switch master
	git pull
	make all

# Plot submissions from component models
plots: pull data covidEnsembles
	Rscript plot_submissions.R

# Build the ensemble
ensemble: pull data covidEnsembles
	Rscript build_ensembles.R
	Rscript build_trained_ensembles.R

# Compare one week ahead forecasts of incident and cumulative deaths at
# national level
incvscum: pull data covidEnsembles
	Rscript inc_vs_cum.R

comparative_plots: ensemble
	Rscript plot_median_vs_trained_ensemble_forecasts.R

git_tag: pull
	git -C $(HUB) tag -a $(TODAY_DATE)-COVIDhub-ensemble -m "$(TODAY_DATE)-COVIDhub-ensemble build inputs"
	git -C $(HUB) push origin $(TODAY_DATE)-COVIDhub-ensemble
