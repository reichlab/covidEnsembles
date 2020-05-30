library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)

expose_stan_functions('bsplines.stan')

n_x <- 101L
x <- seq(from = -0.1, to = 1.1, length=n_x)



#basis <- bspline_basis(
#  n_x = n_x,
#  x = x,
#  order = 1L,
#  n_interior_knots = 9L,
#  boundary_knots = c(0.0, 1.0),
#  interior_knots = seq(from = 0.1, to = 0.9, by = 0.1),
#  natural = 1L
#)
#
#basis_funs <- paste0('basis_', seq_len(ncol(basis)))
#basis_df <- as.data.frame(basis) %>%
#  `colnames<-`(basis_funs) %>%
#  mutate(x = x) %>%
#  tidyr::pivot_longer(all_of(basis_funs), names_to = 'basis_fun', values_to = 'value')
#
#ggplot(data = basis_df, mapping = aes(x = x, y = value)) +
#  geom_line() +
#  facet_wrap( ~ basis_fun)



n_x <- 101L
x <- seq(from = -0.1, to = 1.1, length=n_x)

basis <- bspline_basis(
  n_x = n_x,
  x = x,
  order = 2L,
  n_interior_knots = 9L,
  boundary_knots = c(0.0, 1.0),
  interior_knots = seq(from = 0.1, to = 0.9, by = 0.1),
  natural = 1L
)

basis_funs <- paste0('basis_', seq_len(ncol(basis)))
basis_df <- as.data.frame(basis) %>%
  `colnames<-`(basis_funs) %>%
  mutate(x = x) %>%
  tidyr::pivot_longer(all_of(basis_funs), names_to = 'basis_fun', values_to = 'value')

ggplot(data = basis_df, mapping = aes(x = x, y = value)) +
  geom_line() +
  facet_wrap( ~ basis_fun)




basis <- bspline_basis(
  n_x = n_x,
  x = x,
  order = 4L,
  n_interior_knots = 9L,
  boundary_knots = c(0.0, 1.0),
  interior_knots = seq(from = 0.1, to = 0.9, by = 0.1),
  natural = 1L
)

basis_funs <- paste0('basis_', seq_len(ncol(basis)))
basis_df <- as.data.frame(basis) %>%
  `colnames<-`(basis_funs) %>%
  mutate(x = x) %>%
  tidyr::pivot_longer(all_of(basis_funs), names_to = 'basis_fun', values_to = 'value')

ggplot(data = basis_df, mapping = aes(x = x, y = value, color = basis_fun, group = basis_fun)) +
  geom_line()
