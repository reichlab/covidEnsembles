library(tidyverse)
library(covidEnsembles)
library(ggpubr)
library(here)

plot_losses <- function(
  forecast_date,
  response_var,
  spatial_resolution,
  theta,
  losses, 
  rel_wis
  ) {
  rw <- rel_wis$rel_wis
  names(rw) <- rel_wis$model
  
  wts <- softmax_matrix_rows(-outer(losses$theta, rw))
  
  dat <- bind_cols(losses, as_tibble(wts)) %>%
    pivot_longer(-c(theta, loss)) %>% 
    mutate(loss = ifelse(is.finite(loss), loss, NA))
  
  M <- max(dat$loss, na.rm = TRUE)
  m <- min(dat$loss, na.rm = TRUE)
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
      width = 20 / 200,
      position = position_fill()
    ) +
    geom_line(aes(y = normfun(loss))) +
    geom_vline(xintercept = theta, alpha = .3) +
    scale_y_continuous(
      labels = normfun_inv,
      breaks = normfun(pretty(dat$loss)),
      name = "WIS Loss",
      expand = expansion(0,0),
      position = "right") +
    scale_x_continuous(expand = expansion(0,0)) +
    labs(fill = "") +
    theme(legend.position="left") +
    guides(fill = guide_legend(label.position = "left")) +
    ggtitle(paste(forecast_date, response_var, spatial_resolution)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(p)
}

thetas <- readRDS(file = paste0(here(),
  "/code/application/weekly-ensemble/thetas-", 
  lubridate::floor_date(Sys.Date(), unit = "week") + 1))

thetas <- thetas %>%
  mutate(plots = pmap(
    list(
      forecast_date,
      response_var,
      spatial_resolution,
      theta,
      losses,
      rel_wis
    ),
    plot_losses
  )) 

all <- ggarrange(plotlist = thetas$plots, ncol = 2, nrow = 5, align = "hv")
ggexport(all, 
         filename = paste0(
          here(),
           "/code/application/weekly-ensemble/plots/loss_plot_",
           thetas$forecast_date[1],".pdf"), width=14, height=18)
 