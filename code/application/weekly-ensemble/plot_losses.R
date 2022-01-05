library(tidyverse)
library(covidEnsembles)
library(ggpubr)
library(here)

plot_losses <- function(
  forecast_date,
  response_var,
  spatial_resolution,
  losses, 
  rel_wis
  ) {
  rw <- rel_wis$rel_wis
  names(rw) <- rel_wis$model
  
  wts <- softmax_matrix_rows(-outer(losses$theta, rw))
  
  dat <- bind_cols(losses, as_tibble(wts)) %>%
    pivot_longer(-c(theta, loss))
  
  M <- max(losses$loss)
  m <- min(losses$loss)
  normfun <- function(x) {
    .5 + .8 * ((x - m) / (M - m) - .5)
  }
  
  normfun_inv <- function(y) {
    round((M - m) * ((y - .5) / .8 + .5) + m, 0)
  }
  
  theme_set(theme_minimal())
  p <- ggplot(dat, aes(x = theta)) +
    geom_bar(
      aes(y = value, fill = name),
      stat = "identity",
      alpha = .6,
      width = 20 / 200
    ) +
    geom_line(aes(y = normfun(loss))) +
    scale_y_continuous(
      labels = normfun_inv, name = "WIS Loss",
      position = "right")+
    labs(fill = "") +
    theme(legend.position="left") +
    guides(fill = guide_legend(label.position = "left")) +
    ggtitle(paste(forecast_date, response_var, spatial_resolution))
  return(p)
}

load(file = paste0(here(),
  "/code/application/weekly-ensemble/thetas-", 
  lubridate::floor_date(Sys.Date(), unit = "week") + 1))

thetas <- thetas %>%
  mutate(plots = pmap(
    list(
      forecast_date,
      response_var,
      spatial_resolution,
      losses,
      rel_wis
    ),
    plot_losses
  )) 

all <- ggarrange(plotlist = thetas$plots, ncol = 2, nrow = 5)
ggexport(all, 
         filename = paste0(
          here(),
           "/code/application/weekly-ensemble/plots/loss_plot_",
           thetas$forecast_date[1],".pdf"), width=18, height=24)
 