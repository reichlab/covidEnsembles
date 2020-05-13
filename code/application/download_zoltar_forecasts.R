# download all project forecasts from zoltar
library(zoltr)

zoltar_connection <- new_connection()
zoltr::zoltar_authenticate(zoltar_connection,
  Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

system.time({
  all_forecasts <- covidEnsembles::get_all_project_forecasts(
    zoltar_connection,
    project_url="https://www.zoltardata.com/api/project/44/",
    target %in% paste0(1:4, ' wk ahead cum death'))
})

saveRDS(all_forecasts, './data/all_covid19_forecasts.rds')
