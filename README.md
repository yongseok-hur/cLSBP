# cLSBP

Code for

> Y. Hur, J. Jung and J. Lee. A general Bayesian nonparametric approach for estimating population-level and conditional causal effects. *Observational Studies* (under revision).

The Gibbs sampler is `src/lsbp_1_mixture.cpp`. Scripts are run from the repository root; usage is given at the top of each script. Packages are listed in `R/requirements.R` (`bcf`: `remotes::install_github("jaredsmurray/bcf")`; versions in `docs/`).

| | |
|---|---|
| Section 3 (Figure 1, Tables 1–3, Appendices C–D) | `scripts/sim/` |
| Section 4.1 (Figures 2–5) | `scripts/acic/` (data: [acic2022.mathematica.org](https://acic2022.mathematica.org/)) |
| Section 4.2 (Tables 4–5, Figure 6) | `scripts/pension/` |
| Diagnostics | `scripts/diagnostics/` |

The simulation tables can be rebuilt from `results/` with `scripts/sim/05_make_tables.R`. MCMC fits and the per-dataset ACIC results are not included.

GPL-3.0
