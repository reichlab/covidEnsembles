get_weights <- function(root_dir, estimation_level, quantile_groups, window_size, target_variable, check_missingness_by_target) {
  require(tidyverse)
  path_name <- paste0("intercept_FALSE-combine_method_convex-missingness_impute-", 
                      "quantile_groups_", quantile_groups,
                      "-window_size_", window_size, 
                      "-check_missingness_by_target_", check_missingness_by_target, 
                      "-do_standard_checks_FALSE-do_baseline_check_FALSE/")
  path_to_files <- file.path(root_dir, estimation_level, path_name)
  
  ## get files and datenames
  target_files <- list.files(path_to_files, pattern=paste(target_variable, "*"))
  nchar_target_var <- nchar(target_variable)+1
  dates_of_files <- substr(target_files, start=nchar_target_var+1, stop=nchar_target_var+10)
  
  ## get full filenames
  target_files_full_dir <- list.files(path_to_files, pattern=paste(target_variable, "*"), full.names=TRUE)
  
  ## read in first file
  all_weights <- purrr::map_dfr(target_files,
                                function(x) {
                                  target_file_full_dir <- file.path(path_to_files, x)
                                  nchar_target_var <- nchar(target_variable)+1
                                  date_of_file <- substr(x, start=nchar_target_var+1, stop=nchar_target_var+10)
                                  dat <- read_csv(target_file_full_dir) %>%
                                    mutate(forecast_date = as.Date(date_of_file))
                                  return(dat)
                                })
  return(all_weights)
}

library(tidyverse)
library(plotly)
theme_set(theme_bw())

weights <- get_weights(root_dir = "/Volumes/GoogleDrive/My Drive/COVID19-forecast-hub/COVIDhub-ensemble/retrospective-weights/",
                       estimation_level = "state",
                       quantile_groups = "3_groups",
                       window_size = "full_history",
                       target_variable = "inc_death", 
                       check_missingness_by_target = TRUE)

weights_filtered <- weights %>%
  filter(quantile %in% c(.1, .5, .9), location == "01") %>%
  mutate(model = reorder(model, X=weight), forecast_date = as.Date(forecast_date))

(p <- ggplot(weights_filtered, aes(x=forecast_date, y=weight, fill=model)) +
  geom_bar(stat = "identity") + 
  facet_grid(.~quantile) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

ggplotly(p, width=1000, height=800)


## a few quick summaries
weights_filtered %>% filter(weight>0.01) %>% group_by(forecast_date, quantile) %>% summarize(nmodels=n()) %>% print(n=Inf)
weights_filtered %>% filter(weight>0) %>% group_by(forecast_date, quantile) %>% summarize(nmodels=n()) %>% print(n=Inf)
weights_filtered %>% filter(weight>0) %>% group_by(model, quantile) %>% summarize(nmodels=n()) %>% print(n=Inf)

## looking at weights in recent weeks
weights %>%
  filter(forecast_date > as.Date("2021-01-01")) %>%
  filter(quantile %in% c(.1, .5, .9), location == "01") %>%
  mutate(EW = MMWRweek::MMWRweek(forecast_date)$MMWRweek,
         weight = round(weight,3)) %>%
  select(-location, -forecast_date) %>%
  pivot_wider(names_from = c(quantile, EW), 
              values_from=weight, 
              names_prefix="q", 
              names_sep="_EW", 
              names_sort=TRUE) %>% 
  arrange(desc(q0.5_EW3)) %>%
  print(n=Inf)
  
    