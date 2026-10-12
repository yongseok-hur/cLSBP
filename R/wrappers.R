# Wrapper for BCF (package bcf) with the default settings used in the simulation studies.

get_posterior_bcf_default <- function(y, Z, X, pihat, include_pi = "control", nskip = 0, nsim = 8000, printEvery = 2500){
  bcf(y = y, z = Z, x_control = X, pihat = pihat, nburn = nskip, nsim = nsim, update_interval = printEvery, no_output = T, save_tree_directory = NULL, include_pi = include_pi, n_threads = 1, verbose = FALSE)
}
