# Adapted from https://github.com/anastasiachtz/COMMAND_stan/blob/master/SingleStrainStan.Rmd

functions {
  real[] SIRD(
    real t,       // time
    real[] y,     // system state {susceptible,infected,recovered,dead}
    real[] theta, // parameters
    real[] x_r,
    int[] x_i
  ) {
    real dy_dt[4];

    dy_dt[1] = - theta[1] * y[1] * y[2];
    dy_dt[2] = theta[1] * y[1] * y[2] - theta[2] * y[2] - theta[3] * y[2];
    dy_dt[3] = theta[2] * y[2];
    dy_dt[4] = theta[3] * y[3];

    return dy_dt;
  }
}

data {
  // population size
  int<lower=0> N;

  // number of observation times; assumed to be evenly spaced 1 week apart
  int<lower=0> T;

  // incident deaths at observed times, as counts
  vector[T] y;

  // cumulative deaths at "time 0" as proportion of population
  int<lower=0> d0;


  // quantities that are estimated parameters

  // sird model parameters {beta,gamma,mu} on raw scale,
  // along with location and scale parameters for transformation
  real raw_theta[3];
  real raw_theta_mean[3];
  real raw_theta_sd[3];

  // initial values for s0 and i0 on raw scale,
  // along with location and scale parameters for transformation
  real raw_state_init[2];
  real raw_state_init_mean[2];
  real raw_state_init_sd[2];

  // negative binomial dispersion paramter
  real raw_phi;
  real raw_phi_mean;
  real raw_phi_sd;
}

transformed data {
  // unused quantities for ode integration
  real x_r[0];
  int x_i[0];

  // time points with observations
  int ts[T];

    // quantities that are transformed parameters

  // variable definitions
  // parameters of compartmental model
  real theta[3];

  // solution from the ODE solver
  real state_hat[T, 4];

  // initial conditions for state variables
  real log_denom;
  real temp_state_init[3];
  real state_init[4];

  // parameters of nb distribution
  real yt_mean[T];
  real phi;


  // transformed data
  for(t in 1:T) {
    ts[t] = t;
  }


  // quantities that are transformed parameters

  // variable calculations

  // parameters of compartmental model
  //theta = exp(raw_theta_mean + raw_theta_sd * raw_theta);
  theta = exp(fma(raw_theta_sd, raw_theta, raw_theta_mean));


  // initial conditions for state variables
  for(i in 1:2) {
    temp_state_init[i] = fma(
      raw_state_init_sd[i],
      raw_state_init[i],
      raw_state_init_mean[i]);
  }
  temp_state_init[3] = 0.0;

  // the below is a laborious softmax
  log_denom = log_sum_exp(
    temp_state_init[1],
    log_sum_exp(temp_state_init[2], temp_state_init[3])
  );
  temp_state_init = exp(temp_state_init - log_denom);


  // run ode
  state_init[1] = (1 - d0) * temp_state_init[1];
  state_init[2] = (1 - d0) * temp_state_init[2];
  state_init[3] = (1 - d0) * temp_state_init[3];
  state_init[4] = d0;
  state_hat = integrate_ode_rk45(SIR, y_init, t0, ts, theta, x_r, x_i);

  y_mean = state_hat[:, 4]*N;


  // negative binomial dispersion
  phi = exp(fma(raw_phi_sd, raw_phi, raw_phi_mean));
}

parameters{
  real junk;
}

model {
  junk ~ normal(0, 1);
}

generated quantities {
  // data model
  y ~ neg_binomial_2(y_mean, phi);
}
