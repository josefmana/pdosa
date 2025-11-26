# Set-up targets for regression analyses:
targets_regressions <- list(
  targets::tar_target(
    setup,
    command = setup_regressions(helpers)
  ),
  targets::tar_target(
    regressions,
    command = fit_regressions(preprocessed_data, setup, helpers)
  ),
  targets::tar_target(
    summaries,
    command = summarise_regressions(regressions)
  ),
  targets::tar_target(
    formulas,
    command = set_formulas()
  )
)
