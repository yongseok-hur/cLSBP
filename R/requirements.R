# Packages used by the scripts in this repository.
# bcf is installed from GitHub: remotes::install_github("jaredsmurray/bcf")
# CBPS 0.22 was used for the runs.

# BART (Chipman et al., 2010)
require(BART)
require(BayesTree)

# generalized random forests / causal forest (Wager and Athey, 2018; Athey et al., 2019)
require(grf)

# propensity score estimation: CBPS (Imai and Ratkovic, 2014) and GBM
require(CBPS)
require(gbm)

# data processing
require(dplyr)
require(tidyr)
require(stringr)

# matrix computations
require(Matrix)

# figures
require(gtsummary)
require(ggplot2)
require(ggbreak)
require(ggridges)
require(latex2exp)
require(reshape2)
require(gridExtra)
require(scales)

# LASSO propensity score and the Rcpp sampler
require(glmnet)
require(RcppArmadillo)
require(Rcpp)

# sampling
require(truncnorm)
require(BayesLogit)

# other
require(mvtnorm)
require(data.table)

# Bayesian causal forest (Hahn et al., 2020)
require(bcf)
