# covidEnsembles: make file for building ensembles each week

# 2 processes to do plots and ensemble build in parallel
MAKEFLAGS += -j2

# Define global variables
# This is a directory that is a local clone of reichlab/covid19-forecast-hub
HUB = ../../../../covid19-forecast-hub/

# This is a second local clone used for making pull requests with new ensembles
FHUB = ../../../../fork/covid19-forecast-hub

# current date, for git tags
TODAY_DATE = $(shell date +'%Y-%m-%d')

# Define the full make, making plots and
all: comparative_plots ensemble_trained4 ensemble_main pull data git_tag weight_info

# Update and reinstall covidData package
data:
	cd ../../../../covidData/code/data-processing; make all

# Get the latest forecast files from the hub repository
pull:
	git -C $(HUB) pull origin master

# Tag the hub commit used to build this week's ensembles
git_tag: pull
	git -C $(HUB) tag -a $(TODAY_DATE)-COVIDhub-ensemble -m "$(TODAY_DATE)-COVIDhub-ensemble build inputs"
	git -C $(HUB) push origin $(TODAY_DATE)-COVIDhub-ensemble

# Build the trained and 4 week ensembles
ensemble_trained4: pull data git_tag
	Rscript build_trained_ensembles.R
	Rscript build_4_week_ensembles.R

weight_info: ensemble_trained4
	Rscript -e "rmarkdown::render('fig-ensemble_weight.Rmd', output_file = paste0('plots/weight_reports/fig-ensemble_weight_', Sys.Date(),'.html'))"
	Rscript plot_losses.R

# Build the main ensemble which copies from the trained ensemble
ensemble_main: ensemble_trained4
	Rscript build_ensembles.R

comparative_plots: ensemble_main
	Rscript plot_median_vs_trained_ensemble_forecasts.R

# # Plot submissions from component models
# plots: pull data
# 	Rscript plot_submissions.R
# 	
# Compare one week ahead forecasts of incident and cumulative deaths at
# national level
# incvscum: pull data
# 	Rscript inc_vs_cum.R


# for each ensemble, copy files to fork, push to fork origin, and create pr to upstream
main_pr: 
	(git -C $(FHUB) checkout main || git -C $(FHUB) checkout -b main) && \
	cp forecasts/ensemble-metadata/$(TODAY_DATE)* $(FHUB)/ensemble-metadata/ && \
	cp forecasts/data-processed/COVIDhub-ensemble/$(TODAY_DATE)-COVIDhub-ensemble.csv \
	$(FHUB)/data-processed/COVIDhub-ensemble/ && \
	cd $(FHUB) && \
	git add -A && \
	git commit -m "$(TODAY_DATE) ensemble" && \
	git push origin && \
	gh pr create --title "$(TODAY_DATE) ensemble" --body "Main Ensemble, COVID19 Forecast Hub"

trained_pr: 
	(git -C $(FHUB) checkout trained || git -C $(FHUB) checkout -b trained) && \
	cp forecasts/trained_ensemble-metadata/$(TODAY_DATE)* $(FHUB)/trained_ensemble-metadata/ && \
	cp forecasts/trained_ensemble-metadata/thetas.csv $(FHUB)/trained_ensemble-metadata/thetas.csv && \
	cp forecasts/data-processed/COVIDhub-trained_ensemble/$(TODAY_DATE)-COVIDhub-trained_ensemble.csv \
	$(FHUB)/data-processed/COVIDhub-trained_ensemble/ && \
	cd $(FHUB) && \
	git add -A && \
	git commit -m "$(TODAY_DATE) trained ensemble" && \
	git push origin && \
	gh pr create --title "$(TODAY_DATE) trained ensemble" --body "Trained Ensemble, COVID19 Forecast Hub"

4wk_pr: 
	(git -C $(FHUB) checkout 4wk || git -C $(FHUB) checkout -b 4wk) && \
	cp forecasts/4_week_ensemble-metadata/$(TODAY_DATE)* $(FHUB)/4_week_ensemble-metadata/ && \
	cp forecasts/data-processed/COVIDhub-4_week_ensemble/$(TODAY_DATE)-COVIDhub-4_week_ensemble.csv \
	$(FHUB)/data-processed/COVIDhub-4_week_ensemble/ && \
	cd $(FHUB) && \
	git add -A && \
	git commit -m "$(TODAY_DATE) 4 week ensemble" && \
	git push origin && \
	gh pr create --title "$(TODAY_DATE) 4 week ensemble" --body "4 Week Ensemble, COVID19 Forecast Hub"







