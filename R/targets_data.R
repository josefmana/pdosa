# Set-up targets for data import:
targets_data <- list(
  targets::tar_target(
    files, # For tracking purposes
    command = unlist(c(data_paths(), helper_paths())) |> purrr::keep(\(x) grepl("\\.", x)),
    format = "file",
    cue = targets::tar_cue("always")
  ),
  targets::tar_target(
    datafiles,
    command = data_paths(files),
    cue = targets::tar_cue("always")
  ),
  targets::tar_target(
    helpers,
    command = extract_helpers(helper_paths(files)),
    cue = targets::tar_cue("always")
  ),
  targets::tar_target(
    raw_data,
    command = import_data(datafiles, helpers)
  ),
  targets::tar_target(
    rt_variables,
    command = extract_rt_variables(helpers)
  ),
  targets::tar_target(
    preprocessed_data,
    command = preprocess_data(raw_data, helpers, rt_variables, return = "df")
  ),
  targets::tar_target(
    scales,
    command = preprocess_data(raw_data, helpers, rt_variables, return = "scl")
  ),
  targets::tar_target(
    descriptives,
    command = describe_data(raw_data)
  )
)
