# Load packages required to define the pipeline:
library(targets)
library(patchwork) # for plots arranging

# Set target options:
tar_option_set()

# Load all in-house functions:
tar_source()

# Use multiple cores for model fitting:
options(mc.cores = parallel::detectCores())

# Prepare target list:
list(
  targets_data,
  targets_regressions,
  targets_visualisation
)
