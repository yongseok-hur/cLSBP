# Data-generating processes of Simulations 1 and 2.
#   dgp_Kang_v4 : Simulation 1 (Kang and Schafer (2007) design with targeted selection t)
#   dgp_Hahn_v2 : Simulation 2 (Hahn et al. (2020) design)
# Each function writes one csv file to data/Kang_v4/ or data/Hahn_v2/ (relative to the repository root).

## Simulation 1: Kang and Schafer (2007) design with targeted selection t
ROOM_NUMBER <- 533

dgp_Kang_v4 <- function(seed, selection_option = T, tar_sel = NULL, n = 500){

  # set a seed to make the experiment deterministic
  set.seed(ROOM_NUMBER * seed)

  tar_sel_default = -1.85 / 14
  if(is.null(tar_sel)){
    tar_sel = tar_sel_default
  }

  coeff_mu = c(4, 2, 2, 2)

  coeff_pi_perp = c(-1.0, 0.5, -0.25, -0.1) - tar_sel_default * coeff_mu
  coeff_pi = coeff_pi_perp + tar_sel * coeff_mu

  coeff_tau = c(0, -1, 0, 2)

  X = matrix(rnorm(4 * 500), ncol = 4)

  # X: n x 4 latent covariates, (z1, z2, z3, z4) in Kang and Schafer (2007)

  # mu : prognostic function
  mu <- 10 + c(X %*% coeff_mu)

  # tau : the true treatment effect (heterogeneous)
  tau <- 1 + c(X %*% coeff_tau)

  # pi : the true propensity score
  if(selection_option){
    pi = sigmoid_real(X %*% coeff_pi)
  }else{
    pi = 0.5
  }

  # z : the treatment assignment
  z = rbinom(n, 1, pi)

  # y : n x 1 observed outcomes

  sigma_TR = 1
  c_0 = 1
  y = mu + tau * z + c_0 * sigma_TR * rnorm(n) # additive normal error (random noise)

  # the mis-specified X's denoted X.mis. We assume that data analyst can only observe X.mis (but not X)
  # Please see Kang and Schafer for more details
  X.mis = cbind(
    exp(X[,1] / 2),
    X[,2] / (1 + exp(X[,1])) + 10,
    (X[,1] * X[,3] / 25 + 0.6)^3,
    (X[,2] + X[,4] + 20)^2
  )

  data <- cbind(y, z, X.mis, pi, tau, mu, X)
  colnames(data) <- c('y', 'z', 'x1', 'x2', 'x3', 'x4', 'pi', 'tau', 'mu', 'X1', 'X2', 'X3', 'X4')

  data %>% write.csv(
    file = base::paste0(
      'data/Kang_v4/d', stringr::str_sub(paste0('000', seed), start = -4), '_',
      'tarsel', ifelse(tar_sel < 0, '-', ''),
      ifelse(abs(tar_sel) < 1, paste0('0', round(abs(tar_sel), 3) * 1000), abs(tar_sel)), '_',
      n, '.csv', collapse = ''
    ),
    row.names = FALSE
  )

  base::paste0(
    'data/Kang_v4/d', stringr::str_sub(paste0('000', seed), start = -4), '_',
    'tarsel', ifelse(tar_sel < 0, '-', ''),
    ifelse(abs(tar_sel) < 1, paste0('0', round(abs(tar_sel), 3) * 1000), abs(tar_sel)), '_',
    n, '.csv', collapse = ''
  )
}

dgp_Hahn_v2 <- function(seed, linear = TRUE, hetero = TRUE, n = 250, c_0 = 1) {

  # set a seed to make the experiment deterministic
  set.seed(ROOM_NUMBER * seed)

  # x : n x p covariate matrix
  x = cbind(
    matrix(rnorm(n * 3), ncol = 3), # x1, x2, x3 ~ iid N(0, 1)
    sample(1:3, n, replace = TRUE), # x4 ~ Unif({1, 2, 3})
    sample(0:1, n, replace = TRUE) # x5 ~ Unif({0, 1})
  )

  # tau : the true treatment effect
  tau = 3 # homogeneous
  if(hetero) { # heterogeneous
    tau = 1 + 2 * x[,2] * x[,5] # Hahn et al(2020)
  }

  # mu : the true prognostic function
  g <- function(x_4) { # an auxiliary function to define q
    5 - 3 * x_4
  }

  mu = -6 + g(x[,4]) + 6 * abs(x[,3] - 1) # nonlinear
  if(linear){ # linear
    mu = 1 + g(x[,4]) + x[,1] * x[,3] # Hahn et al(2020)
  }

  # pi : the true propensity score
  pi = 0.8 * pnorm(3 * mu / sd(mu) - 0.5 * x[,1]) + 0.05 + runif(n) / 10

  # z : the treatment assignment
  z = rbinom(n, 1, pi)

  # y : n x 1 observed outcomes
  # noise variance 0.25
  sigma = 0.5
  y = mu + tau * z + sigma * rnorm(n) # additive normal error (random noise)

  data <- cbind(y, z, x, tau, mu, pi)
  colnames(data) <- c('y', 'z', 'x1', 'x2', 'x3', 'x4', 'x5', 'tau', 'mu', 'pi')

  data %>% write.csv(
    file = paste0("data/Hahn_v2/d", stringr::str_sub(paste0('000', seed), start = -4), '_',
                  ifelse(linear, 'lin', 'non'), '_',
                  ifelse(hetero, 'het', 'hom'), '_',
                  n, '.csv')
    ,
    row.names = FALSE
  )

  base::paste0(
    'data/Hahn_v2/d', stringr::str_sub(paste0('000', seed), start = -4), '_',
    ifelse(linear, 'lin', 'non'), '_',
    ifelse(hetero, 'het', 'hom'), '_',
    n, '.csv') %>% cat
}

sigmoid_real <- function(w) {
  1 / (1 + exp(-w))
}
