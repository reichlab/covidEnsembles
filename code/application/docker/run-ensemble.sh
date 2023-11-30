#!/bin/bash

#
# A wrapper script to run the ensemble model, messaging slack with progress and results.
#
# Environment variables (see README.md for details):
# - `SLACK_API_TOKEN`, `CHANNEL_ID` (required): used by slack.sh
# - `GH_TOKEN`, `GIT_USER_NAME`, `GIT_USER_EMAIL`, `GIT_CREDENTIALS` (required): used by load-env-vars.sh
# - `DRY_RUN` (optional): when set (to anything), stops git commit actions from happening (default is to do commits)
#

#
# load environment variables and then slack functions
#

echo "sourcing: load-env-vars.sh"
source "/app/container-utils/scripts/load-env-vars.sh"

echo "sourcing: slack.sh"
source "/app/container-utils/scripts/slack.sh"

#
# start
#

slack_message "starting. id='$(id -u -n)', HOME='${HOME}', PWD='${PWD}', DRY_RUN='${DRY_RUN+x}'"

TODAY_DATE=$(date +'%Y-%m-%d') # e.g., 2022-02-17
OUT_FILE=/tmp/run-ensemble-out.txt
echo -n >${OUT_FILE} # truncate

ENSEMBLES_DIR="/data/covidEnsembles"
WEEKLY_ENSEMBLE_DIR=${ENSEMBLES_DIR}/code/application/weekly-ensemble

# sync fork w/upstream and then push to the fork b/c sometimes a PR will fail to be auto-merged, which we think is
# caused by an out-of-sync fork
HUB_DIR="/data/covid19-forecast-hub" # a fork
cd "${HUB_DIR}"
slack_message "updating forked HUB_DIR=${HUB_DIR}"
git fetch upstream # pull down the latest source from original repo
git checkout master
git merge upstream/master # update fork from original repo to keep up with their changes
git push origin master    # sync with fork

# update covidEnsembles repo
cd ${ENSEMBLES_DIR}
git pull

# update covidData library
slack_message "updating covidData library"
COVID_DATA_DIR="/data/covidData"
git -C ${COVID_DATA_DIR} pull
make -C ${COVID_DATA_DIR}/code/data-processing all

# delete old files
slack_message "deleting old files"
rm -rf ${WEEKLY_ENSEMBLE_DIR}/forecasts/
rm -rf ${WEEKLY_ENSEMBLE_DIR}/plots/
rm -f ${WEEKLY_ENSEMBLE_DIR}/thetas-*

# delete old branches, sync w/upstream, and tag build inputs if not DRY_RUN
if [ -z "${DRY_RUN+x}" ]; then # not DRY_RUN
  # delete old branches
  slack_message "deleting old branches"
  cd "${HUB_DIR}"
  git checkout master
  BRANCHES="primary trained 4wk"
  for BRANCH in ${BRANCHES}; do
    git branch --delete --force ${BRANCH} # delete local branch
    git push origin --delete ${BRANCH}    # delete remote branch
  done

  # tag build inputs
  slack_message "tagging inputs"
  git -C ${HUB_DIR} tag -a ${TODAY_DATE}-COVIDhub-ensemble -m "${TODAY_DATE}-COVIDhub-ensemble build inputs"
  git -C ${HUB_DIR} push origin ${TODAY_DATE}-COVIDhub-ensemble
  git -C ${HUB_DIR} push --tags origin
  git -C ${HUB_DIR} push --tags upstream
fi

#
# build the model via a series of five R scripts
#

cd ${WEEKLY_ENSEMBLE_DIR}
mkdir -p plots/weight_reports

slack_message "running Rscript 1/5: build_trained_ensembles.R"
Rscript build_trained_ensembles.R >>${OUT_FILE} 2>&1

slack_message "running Rscript 2/5: build_4_week_ensembles.R"
Rscript build_4_week_ensembles.R >>${OUT_FILE} 2>&1

slack_message "running Rscript 3/5: build_ensembles.R"
Rscript build_ensembles.R >>${OUT_FILE} 2>&1

slack_message "running Rscript 4/5: fig-ensemble_weight.Rmd"
Rscript -e "rmarkdown::render('fig-ensemble_weight.Rmd', output_file = paste0('plots/weight_reports/fig-ensemble_weight_', Sys.Date(),'.html'))" >>${OUT_FILE} 2>&1

slack_message "running Rscript 5/5: plot_losses.R"
Rscript plot_losses.R >>${OUT_FILE} 2>&1

#
# upload reports
#

slack_message "collecting and uploading reports"

# to find reports we first need the Monday date that the Rscript scripts used when creating files and dirs. we do so
# indirectly by looking for the file loss_plot_${TODAY_DATE}.pdf and then extracting the YYYY-MM-DD date from it. there
# should be exactly one file.

LOSS_PLOT_PDFS=$(find ${WEEKLY_ENSEMBLE_DIR}/plots/loss_plot_*.pdf)
NUM_FILES=0
for PDF_FILE in $LOSS_PLOT_PDFS; do
  ((NUM_FILES++))
done

if [ $NUM_FILES -ne 1 ]; then
  slack_message "PDF_FILE error: not exactly 1 loss plot PDF file. LOSS_PLOT_PDFS=${LOSS_PLOT_PDFS}, NUM_FILES=${NUM_FILES}"
else
  slack_message "PDF_FILE success: PDF_FILE=${PDF_FILE}"
  PDF_FILE_BASENAME=$(basename ${PDF_FILE}) # e.g., "loss_plot_2022-02-21.pdf"
  MONDAY_DATE=${PDF_FILE_BASENAME:10:10}    # substring extraction per https://tldp.org/LDP/abs/html/string-manipulation.html

  # upload the files
  cd ${WEEKLY_ENSEMBLE_DIR}/plots
  UPLOAD_FILES="COVIDhub-4_week_ensemble/${MONDAY_DATE}/*.pdf COVIDhub-ensemble/${MONDAY_DATE}/*.pdf COVIDhub-trained_ensemble/${MONDAY_DATE}/*.pdf weight_reports/fig-ensemble_weight_${TODAY_DATE}.html loss_plot_${MONDAY_DATE}.pdf"
  for UPLOAD_FILE in ${UPLOAD_FILES}; do
    slack_upload ${UPLOAD_FILE}
  done
fi

#
# exit if DRY_RUN
#

if [ -n "${DRY_RUN+x}" ]; then # yes DRY_RUN
  slack_message "DRY_RUN set, exiting"
  exit 0 # success
fi

#
# create PRs if not DRY_RUN
#

cd ${WEEKLY_ENSEMBLE_DIR}

# primary_pr
slack_message "creating primary_pr"
git -C ${HUB_DIR} checkout master &&
  git -C ${HUB_DIR} checkout -b primary &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/ensemble-metadata/* ${HUB_DIR}/ensemble-metadata/ &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/data-processed/COVIDhub-ensemble/* ${HUB_DIR}/data-processed/COVIDhub-ensemble/ &&
  cd ${HUB_DIR} &&
  git add ${HUB_DIR}/ensemble-metadata/ ${HUB_DIR}/data-processed/COVIDhub-ensemble/ &&
  git commit -m "${TODAY_DATE} ensemble" &&
  git push -u origin primary &&
  PR_URL=$(gh pr create --title "${TODAY_DATE} ensemble" --body "Primary Ensemble, COVID19 Forecast Hub")

if [ $? -eq 0 ]; then
  slack_message "primary_pr OK. PR_URL=${PR_URL}"
else
  slack_message "primary_pr failed"
  exit 1 # fail
fi

# trained_pr
slack_message "creating trained_pr"
git -C ${HUB_DIR} checkout master &&
  git -C ${HUB_DIR} checkout -b trained &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/trained_ensemble-metadata/* ${HUB_DIR}/trained_ensemble-metadata/ &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/data-processed/COVIDhub-trained_ensemble/* ${HUB_DIR}/data-processed/COVIDhub-trained_ensemble/ &&
  cd ${HUB_DIR} &&
  git add ${HUB_DIR}/trained_ensemble-metadata/ ${HUB_DIR}/data-processed/COVIDhub-trained_ensemble/ &&
  git commit -m "${TODAY_DATE} trained ensemble" &&
  git push -u origin trained &&
  PR_URL=$(gh pr create --title "${TODAY_DATE} trained ensemble" --body "Trained Ensemble, COVID19 Forecast Hub")

if [ $? -eq 0 ]; then
  slack_message "trained_pr OK. PR_URL=${PR_URL}"
else
  slack_message "trained_pr failed"
  exit 1 # fail
fi

# 4wk_pr
slack_message "creating 4wk_pr"
git -C ${HUB_DIR} checkout master &&
  git -C ${HUB_DIR} checkout -b 4wk &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/4_week_ensemble-metadata/* ${HUB_DIR}/4_week_ensemble-metadata/ &&
  cp ${WEEKLY_ENSEMBLE_DIR}/forecasts/data-processed/COVIDhub-4_week_ensemble/* ${HUB_DIR}/data-processed/COVIDhub-4_week_ensemble/ &&
  cd ${HUB_DIR} &&
  git add ${HUB_DIR}/4_week_ensemble-metadata/ ${HUB_DIR}/data-processed/COVIDhub-4_week_ensemble/ &&
  git commit -m "${TODAY_DATE} 4 week ensemble" &&
  git push -u origin 4wk &&
  PR_URL=$(gh pr create --title "${TODAY_DATE} 4 week ensemble" --body "4 Week Ensemble, COVID19 Forecast Hub")

if [ $? -eq 0 ]; then
  slack_message "4wk_pr OK. PR_URL=${PR_URL}"
else
  slack_message "4wk_pr failed"
  exit 1 # fail
fi

#
# done
#

slack_message "done"
exit 0 # success
