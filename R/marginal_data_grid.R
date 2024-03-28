#' Create data grid
#'
#' Helper to create a data grid, expands modelr::datagrid
#'
#' @param model Model fit from `brm`
#' @param ... Predictor names, optionally set to hold specific levels to predict
#' with
#'
#' @return Data grid with random effects set at NA and predictors set either
#' at a specific value or containing all levels
#' @export
#' @importFrom tidyselect everything
#'
#' @examples
#'
#' # Numeric var center_log_freq set to 0,
#' # Factor var condition uses every level
#' # Factor var tune uses every level
#' # So, you'll get every combination of condition and tune held constant at
#' # centered_log_frequency of 0
#' \dontrun{
#'   marginal_data_grid(bmdl, tune, center_log_freq = 0, condition = 'lower')
#' }
marginal_data_grid <- function(model, ...) {
  model_data <- model$data
  requireNamespace('modelr', quietly = TRUE)
  # Extract set arguments passed to ...
  dots <- rlang::enexprs(...)
  user_args <- list()

  # Wrangle the dots into a named list
  for (i in seq_along(dots)) {
    name_i <- names(dots[i])
    should_expand <- name_i == ""

    # If no value is passed for a variable, use all of its unique levels
    if (should_expand) {
      varname <- as.character(dots[i])
      varval <- unique(model_data[[varname]])
    } else {
      # Otherwise, use the passed value by evaluating it
      varname <- name_i
      varval <- eval(dots[[varname]])
    }

    user_args[[varname]] <- varval
  }

  # Determine which variables were not passed but are expected by the model
  set_vars <- names(user_args)
  mdl_vars <- colnames(model$data)
  is_set <- mdl_vars %in% set_vars
  names(is_set) <- mdl_vars
  # clusters <- unique(mdl$ranef$group)
  grid_vars <- list()

  # If an expected variable was not passed, set it to NA instead, which will
  # marginalize over that predictor. Note that for group-level random effects,
  # you will need to set re_formula = NA in tidybayes::linpred_draws
  for (varname in mdl_vars) {
    grid_vars[[varname]] <- NA
    if (is_set[varname])
      grid_vars[[varname]] <- user_args[[varname]]
  }

  # Remove the dependent variable from the grid
  grid_vars[[model$formula$resp]] <- NULL

  # Pass the composed grid variables to modelr::data_grid
  # Note: data_grid is supposed to do all this if a model is provided to .model,
  #       but it does not appear to work as expected, hence I wrote this fx.
  modelr::data_grid(model_data, ... = !!!grid_vars) |>
    dplyr::summarize(.by = everything()) |>
    dplyr::ungroup()
}
