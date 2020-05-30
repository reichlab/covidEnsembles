functions {
  // forward declaration for recursion
  matrix[] basis_calculation_internal(
    int n_x,
    int order,
    int n_aug_knots,
    vector aug_knots,
    matrix x_m_aug_knot,
    int return_deriv
  );

  // n_interior_knots: number of interior knots
  // boundary_knots: vector of two boundary knots
  // interior_knots: vector of n_interior_knots interior knots
  // order: order of splines, e.g. order 4 is degree 3 splines
  // n_x: number of locations at which to evaluate the spline basis
  // x: locations at which to evaluate the spline basis
  // natural: 1 if natural splines, 0 if not
  //
  // return: n_x by (n_interior_knots + 8 - order) matrix of knots
  matrix bspline_basis(
    int n_x,
    vector x,
    int order,
    int n_interior_knots,
    vector boundary_knots,
    vector interior_knots,
    int natural
  ) {
    int n_aug_knots = n_interior_knots + 8;
    vector[n_aug_knots] aug_knots;
    matrix[n_x, n_aug_knots] x_m_aug_knot;
    matrix[2, n_aug_knots] boundary_m_aug_knot;

    int n_basis = n_aug_knots - order;
    matrix[n_x, n_basis] basis[1];
    matrix[2, n_basis] boundary_basis_and_deriv[2];

    // set up augmented knots: repeat boundary_knots 4 times at edges
    for(i in 1:4) {
      aug_knots[i] = boundary_knots[1];
      aug_knots[4 + n_interior_knots + i] = boundary_knots[2];
    }
    for(i in 1:n_interior_knots) {
      aug_knots[i+4] = interior_knots[i];
    }

    // matrix of differences between x and augmented knots
    for(i in 1:n_aug_knots) {
      x_m_aug_knot[:, i] = x - aug_knots[i];
    }

    basis = basis_calculation_internal(
      n_x,
      order,
      n_aug_knots,
      aug_knots,
      x_m_aug_knot,
      0 // return_deriv
    );
    if(natural == 1) {
      // matrix of differences between boundary knots and augmented knots
      for(i in 1:n_aug_knots) {
        boundary_m_aug_knot[:, i] = boundary_knots - aug_knots[i];
      }

      // values of basis functions and their derivatives at the boundaries
      boundary_basis_and_deriv = basis_calculation_internal(
        2, // n_x
        order,
        n_aug_knots,
        aug_knots,
        boundary_m_aug_knot, // x_m_aug_knot
        1 // return_deriv
      );

      // for observations beyond boundary knots, add linearization of
      // basis functions
      for(i in 1:n_x) {
        for(j in 1:n_basis) {
          if(x[i] < boundary_knots[1]) {
            basis[1, i, j] = basis[1, i, j] +
              boundary_basis_and_deriv[1, 1, j] +
              (x[i] - boundary_knots[1]) * boundary_basis_and_deriv[2, 1, j];
          }

          if(x[i] >= boundary_knots[2]) {
            basis[1, i, j] = basis[1, i, j] +
              boundary_basis_and_deriv[1, 2, j] +
              (x[i] - boundary_knots[2]) * boundary_basis_and_deriv[2, 2, j];
          }
        }
      }
    }

    return basis[1];
  }

  matrix[] basis_calculation_internal(
    int n_x,
    int order,
    int n_aug_knots,
    vector aug_knots,
    matrix x_m_aug_knot,
    int is_boundary
  ) {
    int n_basis = n_aug_knots - order;

    // array of 1 or 2 matrices
    // basis_and_deriv[1] is the basis
    // if is_boundary is 1, basis_and_deriv[2] is the derivative of the basis
    matrix[n_x, n_basis] basis_and_deriv[is_boundary+1];

    // similar structure as above, but for the basis with next lower order
    // (which has 1 more basis function)
    matrix[n_x, n_basis + 1] prev_basis_and_deriv[is_boundary+1];

    // structure for intermediate quantities used in getting from previous order
    // to current order
    matrix[n_x, n_basis + 1] basis_div_knot_diff[is_boundary+1];

    if(order == 1) {
      // different behavior needed depending on whether this calculation is for
      // the boundary points
      if(is_boundary) {
        // initialize to 0
        for(k in 1:(is_boundary+1)) {
          for(i in 1:n_x) {
            for(j in 1:n_basis) {
              basis_and_deriv[k, i, j] = 0.0;
            }
          }
        }

        // only nonzero entries are at the first boundary knots for basis
        basis_and_deriv[1, 1, 4] = 1.0;
        basis_and_deriv[1, 2, n_basis - 4 + 1] = 1.0;
      } else { // not boundary
        // interior basis calculations
        for(i in 1:n_x) {
          for(j in 1:n_basis) {
            basis_and_deriv[1, i, j] =
              x_m_aug_knot[i, j] >= 0.0 && x_m_aug_knot[i, j + 1] < 0.0;
          }
        }
      }
    } else { // order > 1
      // calculate for one lower order
      prev_basis_and_deriv = basis_calculation_internal(
        n_x,
        order-1,
        n_aug_knots,
        aug_knots,
        x_m_aug_knot,
        is_boundary
      );

      // pre-calculate quantities used repeatedly below
      for(j in 1:(n_basis + 1)) {
        real knot_diff = aug_knots[j + order - 1] - aug_knots[j];
        if(knot_diff > 0.00000001) {
          for(k in 1:(is_boundary + 1)) {
            for(i in 1:n_x) {
              basis_div_knot_diff[k, i, j] =
                prev_basis_and_deriv[k, i, j] / knot_diff;
            }
          }
        }
      }

      // actual calculation of basis functions and their derivatives
      // see Eq 5.78 in Elements of Statistical Learning 2E
      for(j in 1:n_basis) {
        // term 1
        if(aug_knots[j + order - 1] - aug_knots[j] > 0.00000001) {
          // calculation for basis
          for(i in 1:n_x) {
            basis_and_deriv[1, i, j] = x_m_aug_knot[i, j]
              * basis_div_knot_diff[1, i, j];
          }

          // calculation for derivative, if applicable
          if(is_boundary) {
            for(i in 1:n_x) {
              basis_and_deriv[2, i, j] = basis_div_knot_diff[1, i, j]
                + x_m_aug_knot[i, j] * basis_div_knot_diff[2, i, j];
            }
          }
        } else {
          // set all to 0.0
          for(k in 1:(is_boundary + 1)) {
            for(i in 1:n_x) {
              basis_and_deriv[k, i, j] = 0.0;
            }
          }
        }

        // term 2
        if(aug_knots[j + order] - aug_knots[j + 1] > 0.00000001) {
          // calculation for basis
          for(i in 1:n_x) {
            basis_and_deriv[1, i, j] -= x_m_aug_knot[i, j + order]
              * basis_div_knot_diff[1, i, j + 1];
          }

          // calculation for derivative, if applicable
          if(is_boundary) {
            for(i in 1:n_x) {
              basis_and_deriv[2, i, j] -= basis_div_knot_diff[1, i, j + 1]
                + x_m_aug_knot[i, j + order] * basis_div_knot_diff[2, i, j + 1];
            }
          }
        }
      }
    }

    return basis_and_deriv;
  }

  real[] SIRD(
    real t,       // time
    real[] state, // system state: susceptible, infected, recovered, dead
    real[] theta, // parameters
    real[] x_r,
    int[] x_i
  ) {
    // quantites related to calculating beta for this time
    int order = 1;
    int n_interior_knots = x_i[1];
    int n_basis = n_interior_knots + 8 - order;
    int n_theta_beta = n_interior_knots + 1; // TODO: update when change order
    vector[1] t_vec;
    vector[2] boundary_knots;
    vector[n_interior_knots] interior_knots;
    matrix[1, n_basis] basis;
    vector[n_basis] b_beta;
    real beta;

    // unpack other parameters
    real gamma = theta[n_theta_beta + 1];
    real mu = theta[n_theta_beta + 2];

    // unpack state
    real s_t = state[1];
    real i_t = state[2];
    real r_t = state[3];
    real d_t = state[4];

    // declare return quantity
    real dstate_dt[4];

    // calculate beta for this time
    t_vec[1] = t;
    for(i in 1:2) {
      boundary_knots[i] = x_r[i];
    }
    for(i in 1:n_interior_knots) {
      interior_knots[i] = x_r[2 + i];
    }
    for(i in 1:3) {
      b_beta[i] = 0.0;
      b_beta[3 + n_theta_beta + i] = 0.0;
    }
    for(i in 1:n_theta_beta) {
      b_beta[3 + i] = theta[i];
    }
    basis = bspline_basis(
      1, // n_x, evaluate at 1 time point t
      t_vec, // x, time point at which we evaluate
      1, // order, order 1 b-spline basis for now
      n_interior_knots,
      boundary_knots,
      interior_knots,
      1 // natural, natural b-spline basis
    );
    beta = to_array_1d(basis * b_beta)[1];

    // calculate derivatives
    dstate_dt[1] = - beta * s_t * i_t;
    dstate_dt[2] = beta * s_t * i_t - gamma * i_t - mu * i_t;
    dstate_dt[3] = gamma * i_t;
    dstate_dt[4] = mu * i_t;

    return dstate_dt;
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

  // knots for spline on beta
  // first two positions are boundary knots, remaining are interior knots
  int n_interior_knots;
  real collected_knots[n_interior_knots+2];
}

transformed data {
  int ode_x_i[1];
  real ts[T+4];
  int n_theta = n_interior_knots + 1 + 2;
  real raw_theta_mean[n_theta];
  real raw_theta_sd[n_theta];
  real raw_state_init_mean[2];
  real raw_state_init_sd[2];
  real raw_phi_mean;
  real raw_phi_sd;

  // set up for ode integration
  // x_i contains number of interior knots
  ode_x_i[1] = n_interior_knots;

  // time points with observations
  for(t in 1:(T+4)) {
    ts[t] = t;
  }


  for(i in 1:(n_interior_knots+1)) {
    raw_theta_mean[i] = 0.33;
    raw_theta_sd[i] = 1.0;
  }
  raw_theta_mean[n_interior_knots + 2] = -0.7;
  raw_theta_sd[n_interior_knots + 2] = 1.0;
  raw_theta_mean[n_interior_knots + 3] = -7.5;
  raw_theta_sd[n_interior_knots + 3] = 1.0;

  raw_state_init_mean[1] = 7.0;
  raw_state_init_mean[2] = 0.0;
  raw_state_init_sd[1] = 0.1;
  raw_state_init_sd[2] = 0.1;
  raw_phi_mean = 1.0;
  raw_phi_sd = 1.0;
}

parameters {
  // sird model parameters {beta,gamma,mu} on raw scale,
  // along with location and scale parameters for transformation
  real raw_theta[n_theta];
//  real raw_theta_mean[3];
//  real raw_theta_sd[3];

  // initial values for s0 and i0 on raw scale,
  // along with location and scale parameters for transformation
  real raw_state_init[2];
//  real raw_state_init_mean[2];
//  real raw_state_init_sd[2];

  // negative binomial dispersion paramter
  real raw_phi;
//  real raw_phi_mean;
//  real raw_phi_sd;
}

transformed parameters {
  // variable definitions
  // parameters of compartmental model
  real theta[n_theta];

  // solution from the ODE solver
  real state_hat[T+4, 4];

  // initial conditions for state variables
  real log_denom;
  real temp_state_init[3];
  real state_init[4];

  // parameters of nb distribution
  real y_mean[T+4];
  real phi;

  // variable calculations

  // parameters of compartmental model
  //theta = exp(raw_theta_mean + raw_theta_sd * raw_theta);
  for(i in 1:n_theta) {
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

  // the below is a laborious softmax
  log_denom = log_sum_exp(
    temp_state_init[1],
    log_sum_exp(temp_state_init[2], temp_state_init[3])
  );
  for(i in 1:3) {
    temp_state_init[i] = exp(temp_state_init[i] - log_denom);
  }

  // run ode
  state_init[1] = (1 - d0) * temp_state_init[1];
  state_init[2] = (1 - d0) * temp_state_init[2];
  state_init[3] = (1 - d0) * temp_state_init[3];
  state_init[4] = d0;
  state_hat = integrate_ode_rk45(
    SIRD,
    state_init,
    0.0,
    ts,
    theta,
    collected_knots,
    ode_x_i);

  y_mean[1] = (state_hat[1, 4] - d0) * N;
  for(t in 2:(T+4)) {
    y_mean[t] = (state_hat[t, 4] - state_hat[t-1, 4])*N;
  }

  //print(y_mean);

  // negative binomial dispersion
  phi = exp(fma(raw_phi_sd, raw_phi, raw_phi_mean));
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
//    y[t] ~ poisson(y_mean[t]);
    y[t] ~ neg_binomial_2(y_mean[t], phi * y_mean[t]);
  }
}

generated quantities {
  // data model
  matrix[T+4, 100] y_pred;
  for(i in 1:100) {
    for(t in 1:(T+4)) {
      y_pred[t, i] = neg_binomial_2_rng(y_mean[t], phi * y_mean[t]);
//      y_pred[t, i] = poisson_rng(y_mean[t]);
    }
  }
}
