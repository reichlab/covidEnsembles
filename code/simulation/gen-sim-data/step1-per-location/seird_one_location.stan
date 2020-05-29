functions {
  real[] SEIRD(
    real t,       // time
    real[] y,     // system state {susceptible,exposed,infected,recovered,dead}
    real[] theta, // parameters
    real[] x_r,
    int[] x_i
  ) {
    real dy_dt[5];

    dy_dt[1] = - theta[1] * y[1] * y[3];
    dy_dt[2] = theta[1] * y[1] * y[3] - theta[2] * y[2];
    dy_dt[3] = theta[2] * y[2] - theta[3] * y[3] - theta[4] * y[3];
    dy_dt[4] = theta[3] * y[3];
    dy_dt[5] = theta[4] * y[3];

    return dy_dt;
  }
}

data {
  // population size
  real<lower=0> N;

  // number of observation times; assumed to be evenly spaced 1 week apart
  int<lower=0> T;

  // incident deaths at observed times, as counts
  int y[T];

  // cumulative deaths at "time 0" as proportion of population
  real<lower=0> d0;
}

transformed data {
  // unused quantities for ode integration
  real x_r[0];
  int x_i[0];

  real raw_theta_mean[4];
  real raw_theta_sd[4];
  real raw_state_init_mean[2];
  real raw_state_init_sd[2];
//  real raw_phi_mean;
//  real raw_phi_sd;

  // time points with observations
  real ts[T+4];
  for(t in 1:(T+4)) {
    ts[t] = t;
  }

  raw_theta_mean[1] = 0.33;
  raw_theta_mean[2] = -0.7; // TODO
  raw_theta_mean[3] = -0.7;
  raw_theta_mean[4] = -7.5;
  raw_theta_sd[1] = 1.0;
  raw_theta_sd[2] = 1.0;
  raw_theta_sd[3] = 1.0;
  raw_theta_sd[4] = 1.0;
  raw_state_init_mean[1] = 7.0;
  raw_state_init_mean[2] = 0.0;
//  raw_state_init_mean[3] = 0.0;
  raw_state_init_sd[1] = 0.1;
  raw_state_init_sd[2] = 0.1;
//  raw_state_init_sd[3] = 0.1;
//  raw_phi_mean = 1000000000.0;
//  raw_phi_sd = 0.1;
}

parameters {
  // sird model parameters {beta,gamma,mu} on raw scale,
  // along with location and scale parameters for transformation
  real raw_theta[4];
//  real raw_theta_mean[3];
//  real raw_theta_sd[3];

  // initial values for s0 and e0 on raw scale,
  // along with location and scale parameters for transformation
  real raw_state_init[2];
//  real raw_state_init_mean[2];
//  real raw_state_init_sd[2];

  // negative binomial dispersion paramter
//  real raw_phi;
//  real raw_phi_mean;
//  real raw_phi_sd;
}

transformed parameters {
  // variable definitions
  // parameters of compartmental model
  real theta[4];

  // solution from the ODE solver
  real state_hat[T+4, 5];

  // initial conditions for state variables
  real log_denom;
  real temp_state_init[4];
  real state_init[5];

  // parameters of nb distribution
  real y_mean[T+4];
//  real phi;


  // variable calculations

  // parameters of compartmental model
  //theta = exp(raw_theta_mean + raw_theta_sd * raw_theta);
  for(i in 1:4) {
    theta[i] = exp(fma(raw_theta_sd[i], raw_theta[i], raw_theta_mean[i]));
  }

  // initial conditions for state variables
  for(i in 1:2) {
    temp_state_init[i] = fma(
      raw_state_init_sd[i],
      raw_state_init[i],
      raw_state_init_mean[i]);
  }
  temp_state_init[3] = 0.0;
  temp_state_init[4] = 0.0;

  // the below is a laborious softmax
  log_denom = log_sum_exp(
    temp_state_init[1],
    log_sum_exp(
      temp_state_init[2],
      log_sum_exp(temp_state_init[3], temp_state_init[4])
    )
  );
  for(i in 1:4) {
    temp_state_init[i] = exp(temp_state_init[i] - log_denom);
  }

  // run ode
  state_init[1] = (1 - d0) * temp_state_init[1];
  state_init[2] = (1 - d0) * temp_state_init[2];
  state_init[3] = (1 - d0) * temp_state_init[3];
  state_init[4] = (1 - d0) * temp_state_init[4];
  state_init[5] = d0;
  state_hat = integrate_ode_rk45(SEIRD, state_init, 0.0, ts, theta, x_r, x_i);

  y_mean[1] = (state_hat[1, 5] - d0) * N;
  for(t in 2:(T+4)) {
    y_mean[t] = (state_hat[t, 5] - state_hat[t-1, 5])*N;
  }

  //print(y_mean);

  // negative binomial dispersion
//  phi = exp(fma(raw_phi_sd, raw_phi, raw_phi_mean));
}

model {
  // priors
  raw_theta ~ normal(0, 1);
//  raw_theta_mean[1] ~ normal(0.33, 0.1);
//  raw_theta_mean[2] ~ normal(-0.7, 0.1);
//  raw_theta_mean[3] ~ normal(-7.5, 0.1);
//  raw_theta_sd ~ gamma(1.0, 10.0);

  raw_state_init ~ normal(0, 1);
//  raw_state_init_mean[1] ~ normal(7.0, 0.1);
//  raw_state_init_mean[2] ~ normal(0.0, 0.1);
//  raw_state_init_sd ~ gamma(1.0, 10.0);

//  raw_phi ~ normal(0, 1);
//  raw_phi_mean ~ normal(1.0, 0.1);
//  raw_theta_sd ~ gamma(1.0, 10.0);

  // data model
  for(t in 1:T) {
    y[t] ~ poisson(y_mean[t]);
//    y[t] ~ neg_binomial_2(y_mean[t], phi);
  }
}

generated quantities {
  // data model
  matrix[T+4, 100] y_pred;
  for(i in 1:100) {
    for(t in 1:(T+4)) {
//      y_pred[t, i] = neg_binomial_2_rng(y_mean[t], phi);
      y_pred[t, i] = poisson_rng(y_mean[t]);
    }
  }
}
