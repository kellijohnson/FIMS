#' Get a tibble of parameter estimates from a fitted model
#' 
#' This function extracts parameter estimates from a fitted TMB model object
#' and reshapes them into a standardized tibble format. It compares the output
#' from TMB with the JSON output.
#' 
#' @param fit A fitted model object returned by `fit_fims()`.
#' @return A tibble containing parameter estimates and related information.
#' \describe{
#'   \item{\code{module_name}:}{The name of the FIMS module (e.g.,
#'     "Data", "Selectivity", "Recruitment", "Growth", "Maturity").}
#'   \item{\code{module_id}:}{}
#'   \item{\code{module_type}:}{}
#'   \item{\code{label}:}{}
#'   \item{\code{type}:}{}
#'   \item{\code{type_id}:}{}
#'   \item{\code{parameter_id}:}{}
#'   \item{\code{fleet}:}{}
#'   \item{\code{year_i}:}{}
#'   \item{\code{age_i}:}{}
#'   \item{\code{length_i}:}{}
#'   \item{\code{input}:}{}
#'   \item{\code{estimated}:}{}
#'   \item{\code{expected}:}{}
#'   \item{\code{observed}:}{}
#'   \item{\code{estimation_type}:}{}
#'   \item{\code{uncertainty}:}{}
#'   \item{\code{distribution}:}{}
#'   \item{\code{input_type}:}{}
#'   \item{\code{lpdf}:}{}
#'   \item{\code{likelihood}:}{}
#'   \item{\code{log_like}:}{}
#'   \item{\code{log_like_cv}:}{}
#'   \item{\code{gradient}:}{}
#' }
#' @examples
#' \dontrun{
#' # Assuming `fit` is a fitted model object from `fit_fims()`
#' estimates <- get_estimates(fit)
#' print(estimates)
#' }
#' @export
get_estimates <- function(fit) {

  # add a check that fit is a valid fitted model object
  if (!is.FIMSFit(fit)) {
    cli::cli_abort(
      "x" = "The provided object is not a valid fitted model object
        of class 'FIMSFit'.",
      "i" = "Please provide a fitted model object returned by
        `fit_fims()`.",
      "i" = "You provided an object of class: {.val {class(fit)}}"
    )
  }
  # Extract the core TMB components (object, sdreport, optimization result)
  # from the fit object.
  obj  <- get_obj(fit)
  sdreport <- get_sdreport(fit)
  opt <- get_opt(fit)
  parameter_names <- get_obj(fit)[["par"]] |>
    names()
  
  # Reshape the TMB output into a standardized data frame.
  # This serves as the "expected" result to compare against.
  tmb_output <- FIMS:::reshape_tmb_estimates(
    obj = obj,
    sdreport = sdreport,
    opt = opt,
    parameter_names = parameter_names
  )

  # Extract the model_output, which contains the JSON-like structure.
  model_output <- get_model_output(fit)
  # Reshape the output from the JSON structure into a data frame.
  json_output <- reshape_json_estimates(model_output)

  # Join the two outputs on parameter_id to compare and consolidate information.
  estimates <- dplyr::left_join(
    json_output,
    tmb_output |>
      dplyr::filter(!is.na(parameter_id)) |>
      dplyr::select(-initial, -module_name, -module_id, -estimate, -label),
    by = c("parameter_id")
  ) |>
    dplyr::mutate(
      uncertainty = dplyr::coalesce(uncertainty.x, uncertainty.y),
      .after = "estimation_type"
    ) |>
    dplyr::select(-uncertainty.x, -uncertainty.y)
}