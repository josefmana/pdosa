#' Adjust for Multiple Comparisons
#'
#' Evaluate a vector of p-values for statistical significance on
#' 5% False Discovery Rate according to the Benjamini-Hochberg
#' procedure.
#'
#' @param p A vector of p-values.
#'
#' @returns A vector indicating statistical significance after
#'    adjustment.
#'
#' @export
bh_adjust <- function(p) {
  # Extract threshold:
  bh_thres <- data.frame(
    p = sort(p), # sort p-values from smallest to largest
    thres = .05 * (seq_along(p)) / length(p) # prepare BH thresholds for each p-value
  ) |>
    dplyr::mutate(sig = dplyr::if_else(p <= thres, TRUE, FALSE)) |>
    dplyr::filter(sig == TRUE) |>
    dplyr::select(thres) |>
    max() |>
    suppressWarnings() # for cases with nothing significant
  # Return stars based on this threshold:
  dplyr::if_else(p < bh_thres, "*", "")
}
