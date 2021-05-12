library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(zeallot)
library(covidEnsembles)

args <- commandArgs(trailingOnly = TRUE)
run_setting <- args[1]

if (is.na(run_setting)) {
  run_setting <- "mghpcc_cluster"
#  stop("no run setting")
#  run_setting <- "cluster"
  #run_setting <- "local"
  #num_local_cores <- 16L
}

output_path <- "code/application/retrospective-qra-comparison/log/"

# define analysis combinations to run
if (run_setting == "midas_cluster_single_node") {
  # number of available cores on cluster node
  num_cores <- 39L

  job_ind <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

  analysis_combinations <- readr::read_csv(
    "code/application/retrospective-qra-comparison/analysis_combinations.csv"
  )
  first_ind <- (job_ind - 1) * 39 + 1
  if (first_ind > nrow(analysis_combinations)) {
    analysis_combinations <- NULL
  } else {
    analysis_combinations <- analysis_combinations[
      seq(from = first_ind, length = min(39, nrow(analysis_combinations))), ,
      drop = FALSE]
  }
} else {
  # number of cores to use for local runs
  num_cores <- 18L

  first_forecast_date <- lubridate::ymd("2020-06-22")
  last_forecast_date <- lubridate::ymd("2021-04-26")
#  first_forecast_date <- lubridate::ymd("2020-12-07")
#  last_forecast_date <- lubridate::ymd("2020-12-07")

  #last_forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1
  num_forecast_weeks <-
    as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

  trained_analysis_combinations <- tidyr::expand_grid(
#    spatial_resolution = c("county", "state", "national", "state_national"),
    spatial_resolution = c("state"),
#    response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
    response_var = c("inc_case", "inc_death"),
    forecast_date = as.character(
      lubridate::ymd(first_forecast_date) +
        seq(from = 0, length = num_forecast_weeks) * 7),
    intercept = c("FALSE"),
    combine_method = "convex_median",
#    combine_method = c("convex", "convex_median"),
#    quantile_group_str = c("per_quantile", "3_groups", "per_model"),
#    quantile_group_str = c("3_groups"),
    quantile_group_str = c("per_model"),
    noncross = "sort",
#    missingness = c("mean_impute"),
    missingness = "renormalize",
#    missingness = "none",
#    window_size = c(as.character(3:10), "full_history"),
#    window_size = c(as.character(c(4, 8, 12)), "full_history"),
    window_size = "full_history",
#    top_models = "0",
    top_models = c("5", "10"),
    check_missingness_by_target = "TRUE",
    do_standard_checks = "FALSE",
    do_baseline_check = "FALSE"
  ) %>%
    dplyr::filter(
      (response_var %in% c("cum_death", "inc_death") &
        spatial_resolution != "county" &
        forecast_date >= "2020-06-22") |
      (response_var == "inc_case" & forecast_date >= "2020-09-14") |
      (response_var == "inc_hosp" & forecast_date >= "2020-11-23" &
        spatial_resolution != "county"),
      (spatial_resolution != "county") | window_size %in% c("3", "4")
    ) %>%
    dplyr::arrange(window_size, forecast_date)

  unweighted_analysis_combinations <- tidyr::expand_grid(
#    spatial_resolution = c("county", "state", "national"),
    spatial_resolution = c("state"),
#    response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
    response_var = c("inc_case", "inc_death"),
    forecast_date = as.character(
      lubridate::ymd(first_forecast_date) +
        seq(from = 0, length = num_forecast_weeks) * 7),
    intercept = c("FALSE"),
    combine_method = c("ew", "median"),
    quantile_group_str = c("per_model"),
    noncross = "constrain",
    missingness = c("by_location_group"),
    window_size = "0",
    top_models = "0",
    check_missingness_by_target = "FALSE",
    do_standard_checks = "FALSE",
    do_baseline_check = "FALSE"
  ) %>%
    dplyr::filter(
      (response_var %in% c("cum_death", "inc_death") &
        spatial_resolution != "county" &
        forecast_date >= "2020-06-22") |
      (response_var == "inc_case" & forecast_date >= "2020-09-14") |
      (response_var == "inc_hosp" & forecast_date >= "2020-11-23" &
        spatial_resolution != "county")
    )

  analysis_combinations <- dplyr::bind_rows(
    trained_analysis_combinations#,
#    unweighted_analysis_combinations
  )

  # filter to keep only cases that have not successfully run previously
  analysis_combinations <- analysis_combinations %>%
    dplyr::mutate(
      case_str = paste0(
        "intercept_", as.character(intercept),
        "-combine_method_", combine_method,
#        "-missingness_", ifelse(missingness == "mean_impute", "impute", missingness),
        "-quantile_groups_", quantile_group_str,
#	"-noncross_", noncross,
        "-window_size_", window_size,
        "-top_models_", top_models#,
#        "-check_missingness_by_target_", check_missingness_by_target,
#        "-do_standard_checks_", do_standard_checks,
#        "-do_baseline_check_", do_baseline_check
      ),
      spatial_resolution_path = dplyr::case_when(
        spatial_resolution == "all" ~ "",
        TRUE ~ spatial_resolution),
      forecasts_dir = file.path(
        "code/application/retrospective-qra-comparison/retrospective-forecasts",
        spatial_resolution_path,
        case_str),
      forecast_filename = paste0(
        forecasts_dir, "/",
        response_var, "-", forecast_date, "-",
        case_str, ".csv"),
      job_complete = file.exists(forecast_filename)) %>%
    dplyr::filter(!job_complete)

  dim(analysis_combinations)
}

# create jobs to run all analysis combinations
if (run_setting %in% c("local", "midas_cluster_single_node")) {
  library(doParallel)
  registerDoParallel(cores = num_cores)

  run_status <- foreach(row_ind = seq_len(rev(nrow(analysis_combinations)))) %dopar% {
  # foreach(row_ind = seq_len(2)) %dopar% {
    response_var <- analysis_combinations$response_var[row_ind]
    forecast_date <- analysis_combinations$forecast_date[row_ind]
    intercept <- analysis_combinations$intercept[row_ind]
    combine_method <- analysis_combinations$combine_method[row_ind]
    quantile_group_str <- analysis_combinations$quantile_group_str[row_ind]
    noncross <- analysis_combinations$noncross[row_ind]
    missingness <- analysis_combinations$missingness[row_ind]
    window_size <- analysis_combinations$window_size[row_ind]
    top_models <- analysis_combinations$top_models[row_ind]
    check_missingness_by_target <-
      analysis_combinations$check_missingness_by_target[row_ind]
    do_standard_checks <- analysis_combinations$do_standard_checks[row_ind]
    do_baseline_check <- analysis_combinations$do_baseline_check[row_ind]
    spatial_resolution <- analysis_combinations$spatial_resolution[row_ind]

    run_cmd <- paste0(
      "R CMD BATCH --vanilla \'--args ",
      run_setting, " ",
      response_var, " ",
      forecast_date, " ",
      intercept, " ",
      combine_method, " ",
      missingness, " ",
      quantile_group_str, " ",
      noncross, " ",
      window_size, " ",
      top_models, " ",
      check_missingness_by_target, " ",
      do_standard_checks, " ",
      do_baseline_check, " ",
      spatial_resolution,
      "\' code/application/retrospective-qra-comparison/fit_retrospective_model.R ",
      output_path, "output-", response_var, "-", forecast_date, "-",
      intercept, "-", combine_method, "-", missingness, "-", quantile_group_str,
      "-", noncross, "-", window_size, "-", top_models, "-",
      check_missingness_by_target, "-", do_standard_checks, "-",
      do_baseline_check, "-", spatial_resolution, ".Rout")

    system(run_cmd)
  }
} else if (run_setting == "midas_cluster") {
  # output analysis combinations to csv -- each job will use arguments from one
  # row of this csv to determine run settings
  readr::write_csv(
    analysis_combinations,
    "code/application/retrospective-qra-comparison/analysis_combinations.csv")

  shell_script_cmds <- paste0(
    "#!/bin/bash\n",
    "#\n",
    "#SBATCH --job-name=run_fit_retrospective_model\n",
    "#SBATCH --output=run_fit_retrospective_model_arr_%A_%a.out\n",
    "#SBATCH --error=run_fit_retrospective_model_arr_%A_%a.err\n",
    "#SBATCH --array=1-2\n",
#    "#SBATCH --array=1-", min(1000, nrow(analysis_combinations)), "\n",
    "#SBATCH --time=36:00:00\n",
#    "#SBATCH --partition=n1-standard-32\n",
    "#SBATCH --partition=n1-ultramem-40\n",
    "#SBATCH --ntasks=1\n",
#    "#SBATCH --mem-per-cpu=3G\n",
    "\n",
    "module purge\n",
#    "module load gcc/9.1.0\n",
#    "module load python/anaconda3.7-2019.03\n",
#    "module load r/4.0.3_no-MPI\n",
#    "module load gurobi\n",
    "module load singularity/singularity-3.6.2\n",
    "singularity shell code/application/singularity/singularity_tfp_cpu.sif\n",
#    "\n",
    "R CMD BATCH --vanilla \'--args midas_cluster_single_node\' ",
      "code/application/retrospective-qra-comparison/run_fit_retrospective_model.R ",
      output_path, "output-$SLURM_ARRAY_TASK_ID.Rout"
  )

  submit_slurm_script <-
    "code/application/retrospective-qra-comparison/submit_fit_retrospective_model.slurm"
  cat(shell_script_cmds, file = submit_slurm_script)

  system(paste0("sbatch ", submit_slurm_script))
} else if (run_setting == "mghpcc_cluster") {
  for(row_ind in seq_len(nrow(analysis_combinations))) {
#  for(row_ind in seq_len(2)) {
    response_var <- analysis_combinations$response_var[row_ind]
    forecast_date <- analysis_combinations$forecast_date[row_ind]
    intercept <- analysis_combinations$intercept[row_ind]
    combine_method <- analysis_combinations$combine_method[row_ind]
    quantile_group_str <- analysis_combinations$quantile_group_str[row_ind]
    noncross <- analysis_combinations$noncross[row_ind]
    missingness <- analysis_combinations$missingness[row_ind]
    window_size <- analysis_combinations$window_size[row_ind]
    top_models <- analysis_combinations$top_models[row_ind]
    check_missingness_by_target <-
      analysis_combinations$check_missingness_by_target[row_ind]
    do_standard_checks <- analysis_combinations$do_standard_checks[row_ind]
    do_baseline_check <- analysis_combinations$do_baseline_check[row_ind]
    spatial_resolution <- analysis_combinations$spatial_resolution[row_ind]

    model_case <- paste0(
      response_var,
      "-", forecast_date,
      "-", as.character(intercept),
      "-", combine_method,
#      "-", ifelse(missingness == "mean_impute", "impute", missingness),
      "-", quantile_group_str,
#      "-", noncross,
      "-", window_size,
      "-", top_models,
#      "-", check_missingness_by_target,
#      "-", do_standard_checks,
#      "-", do_baseline_check,
      "-", spatial_resolution)
    
    filename <- paste0(
      "code/application/retrospective-qra-comparison/submit_fit_retrospective_model_",
      model_case, ".sh")

    if (combine_method == "convex_median" && window_size == "full_history") {
      num_cores <- "8"
      memory <- "8000"
    } else {
      num_cores <- "1"
      if (window_size == "full_history") {
        memory <- "40000"
      } else {
        memory <- as.character(as.numeric(window_size) * 3000)
      }
    }
    
    requestCmds <- "#!/bin/bash\n"
    requestCmds <- paste0(requestCmds, "#BSUB -n ", num_cores, " # how many cores we want for our job\n",
                "#BSUB -R span[hosts=1] # ask for all the cores on a single machine\n",
                "#BSUB -R rusage[mem=", memory, "] # ask for memory\n",
                "#BSUB -o covidEnsembles_new.out # log LSF output to a file\n",
                "#BSUB -W 72:00 # run time\n",
                "#BSUB -q long # which queue we want to run in\n")
    
    cat(requestCmds, file = filename)
#    cat("module load gcc/8.1.0\n", file = filename, append = TRUE)
#    cat("module load R/4.0.0_gcc\n", file = filename, append = TRUE)
#    cat("module load gurobi/900\n", file = filename, append = TRUE)
    cat("module load singularity/singularity-3.6.2\n", file = filename, append = TRUE)
    cat(paste0(
        "singularity exec code/application/singularity/singularity_tfp_cpu.sif ",
        "R CMD BATCH --vanilla \'--args ",
        "cluster_single_node ",
        response_var, " ",
        forecast_date, " ",
        intercept, " ",
        combine_method, " ",
        missingness, " ",
        quantile_group_str, " ",
	noncross, " ",
        window_size, " ",
        top_models, " ",
        check_missingness_by_target, " ",
        do_standard_checks, " ",
        do_baseline_check, " ",
        spatial_resolution,
        "\' code/application/retrospective-qra-comparison/fit_retrospective_model.R ",
        output_path, "output-", response_var, "-", forecast_date, "-",
        intercept, "-", combine_method, "-", missingness, "-", quantile_group_str,
        "-", noncross, "-", window_size, "-", top_models, "-", check_missingness_by_target, "-",
        do_standard_checks, "-", do_baseline_check, "-", spatial_resolution,
        ".Rout"),
      file = filename, append = TRUE)
    
    bsubCmd <- paste0("bsub < ", filename)
    system(bsubCmd)
  }
}
