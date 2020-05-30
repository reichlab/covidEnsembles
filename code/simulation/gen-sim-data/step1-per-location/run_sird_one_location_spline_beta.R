library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(foreach)
library(doParallel)

rstan_options(auto_write = TRUE)

registerDoParallel(30)


covid <- read_csv('../../../../data-raw/jhu-incident.csv')
pops <- read_csv('../../../../data-raw/location_traits.csv')

all_locations <- unique(covid$location_abbreviation)

compiled <- rstan::stan_model("sird_one_location_spline_beta.stan")

foreach(loc = all_locations, .packages=c('dplyr', 'rstan')) %dopar% {
  rstan_options(auto_write = TRUE)

  loc_covid <- covid %>%
    dplyr::filter(location_abbreviation==loc)

  fit <- stan(
    file = "sird_one_location_spline_beta.stan",
    data = list(
      N = pops$totalpop[pops$postalCode == loc],
      T = nrow(loc_covid),
      y = loc_covid$value,
      d0 = 0.0,
      n_interior_knots=4L,
      collected_knots=c(2.0, 17.0, seq(from = 5.0, to = 14.0, by = 3.0))
    ),
    init = list(list(
      raw_theta = rep(0.0, 4+1+2),
      raw_state_init = c(0.0, 0.0),
      raw_phi = 0.0
    )),
    iter = 1000,
    chains = 1,
    seed = 894711,
    verbose = TRUE)

  all_samples <- rstan::extract(fit)

  saveRDS(all_samples, paste0('posterior_samples/all_samples_', loc, '.rds'))
}

to_plot <- purrr::map_dfr(
  all_locations,
  function(loc) {
  loc_covid <- covid %>%
    dplyr::filter(location_abbreviation==loc)

  all_samples <- readRDS(paste0('posterior_samples/all_samples_', loc, '.rds'))

  preds_df <- purrr::map_dfr(
    seq_len(500),
    function(i) {
      purrr::map_dfr(
        seq_len(100),
        function(j) {
          data.frame(
            week = seq_len(22),
            y_hat = all_samples$y_pred[i, 1:22, j]
          )
        }
      )
    }
  )

  qs <- c('0.010', '0.025', '0.100', '0.250', '0.750', '0.900', '0.975', '0.990')
  pred_intervals <- purrr::map_dfr(
    qs,
    function(prob) {
      preds_df %>%
        dplyr::group_by(week) %>%
        dplyr::summarize(
          value = quantile(y_hat, probs = as.numeric(prob), na.rm=TRUE)
        ) %>%
        dplyr::mutate(
  #        quantile = prob,
          alpha = ifelse(
            as.numeric(prob) < 0.5,
            prob,
            format(1 - as.numeric(prob), digits = 3, nsmall = 3)
          ),
          interval_type = ifelse(
            as.numeric(prob) < 0.5,
            'lower',
            'upper'
          )
        )
    }
  ) %>%
  tidyr::pivot_wider(
    id_cols = c('week', 'alpha'),
    names_from = 'interval_type',
    values_from = 'value'
  ) %>%
  mutate(location = loc)
})

pdf('ny_preds_spline_beta.pdf', width=48, height=24)
ggplot() +
  geom_ribbon(
    data = to_plot,
    mapping = aes(x = week, ymin = lower, ymax = upper, fill = alpha)
  ) +
  geom_point(
    data = covid %>% mutate(location = location_abbreviation),
    mapping = aes(x = as.numeric(week) - 3, y = value),
    color = "cornflowerblue"
  ) +
  geom_line(
    data = covid %>% mutate(location = location_abbreviation),
    mapping = aes(x = as.numeric(week) - 3, y = value),
    color = "cornflowerblue"
  ) +
  scale_fill_viridis_d(begin = 0.25, option = 'B') +
  facet_wrap( ~ location, scales = 'free_y') +
  theme_bw()
dev.off()



ny_covid <- covid %>%
  dplyr::filter(location_abbreviation=='NY')

fit_sample <- stan(
  file = "sird_one_location_spline_beta.stan",
  data = list(
    N = 19616658.0,
    T = 18,
    y = c(0, 0, 0, 0, 0, 0, 0, 6, 189, 1393, 4403, 6507, 6262, 3249, 2189, 2414, 1437, 982),
    d0 = 0.0,
    n_interior_knots=4L,
    collected_knots=c(2.0, 17.0, seq(from = 5.0, to = 14.0, by = 3.0))
  ),
  init = list(list(
    raw_theta = rep(0.0, 4+1+2),
    raw_state_init = c(0.0, 0.0),
    raw_phi = 0.0
  )),
  iter = 1000,
  chains = 1,
  seed = 894711,
  verbose = TRUE)


preds <- rstan::extract(fit_sample, 'y_pred')
preds_df <- purrr::map_dfr(
  seq_len(500),
  function(i) {
    purrr::map_dfr(
      seq_len(100),
      function(j) {
        data.frame(
          week = seq_len(22),
          y_hat = preds$y_pred[i, 1:22, j]
        )
      }
    )
  }
)

qs <- c('0.010', '0.025', '0.100', '0.250', '0.750', '0.900', '0.975', '0.990')
pred_intervals <- purrr::map_dfr(
  qs,
  function(prob) {
    preds_df %>%
      dplyr::group_by(week) %>%
      dplyr::summarize(
        value = quantile(y_hat, probs = as.numeric(prob))
      ) %>%
      dplyr::mutate(
#        quantile = prob,
        alpha = ifelse(
          as.numeric(prob) < 0.5,
          prob,
          format(1 - as.numeric(prob), digits = 3, nsmall = 3)
        ),
        interval_type = ifelse(
          as.numeric(prob) < 0.5,
          'lower',
          'upper'
        )
      )
  }
) %>%
tidyr::pivot_wider(
  id_cols = c('week', 'alpha'),
  names_from = 'interval_type',
  values_from = 'value'
)

pdf('ny_preds_spline_beta.pdf')
ggplot() +
  geom_ribbon(
    data = pred_intervals,
    mapping = aes(x = week, ymin = lower, ymax = upper, fill = alpha)
  ) +
  geom_point(
    data = ny_covid,
    mapping = aes(x = as.numeric(week) - 3, y = value),
    color = "cornflowerblue"
  ) +
  geom_line(
    data = ny_covid,
    mapping = aes(x = as.numeric(week) - 3, y = value),
    color = "cornflowerblue"
  ) +
  scale_fill_viridis_d(begin = 0.25, option = 'B') +
  theme_bw()
dev.off()
