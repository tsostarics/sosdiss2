#' Use posterior parameter estimates as new prior distribution
#'
#' Given a brmsfit object, convert each fixed effect parameter into a list of
#' prior specifications that can be passed to a new model. All parameters are
#' specified as a normal distribution with mean and standard deviation based
#' on the draws from all chains of the model.
#'
#' @param bmdl Model fit with brm
#' @param scale Numeric multiplier to apply to the standard deviation of the
#' normal distributions. Set higher than 1 to make priors that are a bit weaker.
#' @param shift_to_zero Logical, default FALSE, whether to override the mean for
#' each prior to be centered at 0 rather than the estimated mean. Useful if you
#' want regularizing priors with the standard deviation tuned to the posterior
#' distribution's variance.
#'
#' @return List of prior() calls
#' @export
posterior_as_new_prior_dists <- function(bmdl, scale = 1, shift_to_zero = FALSE) {
  # Extract all the coefficients from the model, note that this will include
  # all of the MANY random effect terms, so we filter to just the b and sd
  # coefficients
  model_parameters <- variables(bmdl)
  model_parameters <- model_parameters[grepl("^(b_)|^(sd_)", model_parameters)]
  par_indices <- seq_along(model_parameters)

  # The extracted coefficient names include information about what kind of
  # coefficient it is. We need to extract each piece of information from the name
  # so that we can pass it in as an individual argument to a prior() call
  par_classes <- stringr::str_match(model_parameters, "^(b|sd)_?")[,2]
  par_groups  <- stringr::str_match(model_parameters, "^(b|sd)_(.+(?!=__))__")[,3]

  # We need to check for distributional parameters using a nonexported helper,
  # otherwise we run into issues when coefficient names have underscores.
  par_dpars <- model_parameters[NA]
  valid_dpars <- utils::getFromNamespace('valid_dpars', 'brms')
  formula_dpars <- valid_dpars(bmdl)
  formula_dpars <- formula_dpars[formula_dpars != "mu"] # mu isn't specified, only things like disc or sigma
  if (length(formula_dpars) > 0) {
    dpar_pattern <- paste0("__(", paste0(formula_dpars, collapse = "|"), ")")
    par_dpars   <- stringr::str_match(model_parameters, dpar_pattern)[,2]
  }


  # Given the parts we extracted, go from left to right in the full name and
  # remove each part we no longer need. The remaining piece is the coef name.
  par_coefs <-
    vapply(par_indices,
           \(i) {
             model_parameters[i] |>
               gsub(paste0("^", par_classes[i], "_"), "", x=_) |>
               gsub(paste0("^", par_groups[i], "__"), "", x=_) |>
               gsub(paste0("^", par_dpars[i], "_"), "", x=_)
           }, FUN.VALUE = "char")

  # Fit a normal distribution to each posterior distribution, then construct
  # a prior() call using the information we extracted above
  prior_list <-
    lapply(par_indices,
         \(i) {
           # Use fitdistr on all the chains to get a mean and standard deviation
           normal_params <- MASS::fitdistr(unlist(posterior::as_draws(bmdl,
                                                                      variable = model_parameters[i])),
                                           'normal')
           estimates <- unname(normal_params$estimate)
           if (shift_to_zero)
             estimates[1L] <- 0

           # Create a normal(m, sd) call to pass to prior. `normal` is not an R
           # function, so it needs to be quoted.
           normal_call <- as.call(c(quote(normal),
                                    list(estimates[1L],
                                         estimates[2L]*scale)))

           # Add in the parts we extracted
           prior_args <- list(prior = normal_call,
                              class = par_classes[i],
                              coef = par_coefs[i])

           # Only include group and dpar if it's explicitly specified/relevant
           if (!is.na(par_groups[i]))
             prior_args[['group']] <- par_groups[i]

           if (!is.na(par_dpars[i]))
             prior_args[['dpar']] <- par_dpars[i]

           # The intercept is listed as a b coefficient but the prior isn't
           # actually specified that way, so we need to fix just that one
           if (model_parameters[i] == 'b_Intercept') {
             prior_args[['class']] <- "Intercept"
             prior_args[['coef']] <- NULL
           }

           do.call(brms::prior, prior_args)
         })

  do.call(rbind, prior_list)
}


#' Convert prior specification to dataframe
#'
#' Given a list of priors, convert them to a dataframe.
#' brms represents priors as dataframes anyways, but this adds a few additional
#' columns of information. Namely, the full coefficient name, the distribution
#' used, and the arguments to that distribution as a list column.
#'
#' @param prior_specs List of prior()
#'
#' @return Dataframe with prior information
#' @export
unpack_prior_specification <- function(prior_specs) {
  prior_specs <- as_prior_list(prior_specs)

  # Convert priors to dataframes and add the full name of the coefficient
  prior_info_df <-
    cbind(
      full_name = get_coef_from_prior(prior_specs),
      do.call(rbind, args = lapply(prior_specs, as.data.frame)))

  # Extract the distribution name and its parameters and wrap it in a data frame
  prior_distribution_info <-
    lapply(prior_specs, \(ps) {
      prior_expr <- parse(text = ps[['prior']])[[1L]]

      prior_dist <- as.character(prior_expr[[1L]])
      prior_args <-
        lapply(prior_expr, \(x) tryCatch(eval(x), error = \(e) as.character(x)))


      data.frame(distribution = prior_dist,
                 args = I(list(c(prior_args[-1], recursive = T))))
    })

  cbind(prior_info_df,
        do.call(rbind, args = prior_distribution_info))

}

#' Title
#'
#' @param prior_specs brmsprior object
#'
#' @return List of brmsprior objects
#' @export
as_prior_list <- function(prior_specs) {
  if (inherits(prior_specs, 'brmsprior')) {
    if (nrow(prior_specs) > 1){
      prior_specs <- lapply(seq_len(nrow(prior_specs)), \(i) prior_specs[i,])
    } else {
      prior_specs <- list(prior_specs)
    }
  }

  prior_specs
}


#' Extract full coefficient name from the prior
#'
#' @param prior_specs List of prior() calls
#'
#' @return Character vector of coefficient names
#' @export
get_coef_from_prior <- function(prior_specs) {
  prior_specs <- as_prior_list(prior_specs)
  # brms has unexported functions that format the coefficient name given a prior
  # so we're just going to borrow those.
  .print_prior <- utils::getFromNamespace('.print_prior', 'brms')
  prepare_print_prior <- utils::getFromNamespace('prepare_print_prior', 'brms')

  prior_strings <-
    vapply(prior_specs,
           \(prior_spec) {
             .print_prior(prepare_print_prior(prior_spec))
           }, "char")

  # The whole string is formatted as coef_name ~ dist(...)
  # we only need the first part
  gsub(" ~ .+$", "", prior_strings)
}

#' Drop duplicated prior specifications
#'
#' brm will throw an error if multiple priors are explicitly set for the same
#' parameter. This helper uses `brms:::duplicated.brmsprior` to filter out any
#' duplicates.
#'
#' @param priors Object of type `brmsprior`
#'
#' @return brmsprior without duplicates. Note that the first instance in the
#' brmsprior dataframe is retained.
#' @export
drop_duplicated_priors <- function(priors) {
  stopifnot(inherits(priors, 'brmsprior'))
  is_duplicate_prior <- utils::getFromNamespace('duplicated.brmsprior', 'brms')

  priors[!is_duplicate_prior(priors),]
}

#' Plot normal approximations of the posterior distribution
#'
#' This is a helper to plot normal distributions on top of of the plot created
#' by `plot(bayestestR::hdi(model))`. Useful when you're using the posterior
#' distribution of some parameter as the prior for another model, but with the
#' standard deviation scaled or the mean shifted to zero.
#'
#' @param bmdl brmsfit object
#' @param params Which parameters to restrict the plot to
#' @param scale Numeric multiplier for the standard deviations of parameters
#' @param shift_to_zero Whether to shift the means of the posterior normal
#' approximations to 0
#'
#' @return List of geoms to add to the hdi plot
#' @export
show_posterior_approximations <- function(bmdl,params = '', scale=1, shift_to_zero=FALSE) {
  # Get the HDI for the specified parameters
  posterior_hdi <- bayestestR::hdi(bmdl, parameters = params)

  # Approximate posterior distributions for each parameter using normal distributions
  prior_info <-
    unpack_prior_specification(posterior_as_new_prior_dists(bmdl, scale, shift_to_zero)) |>
    dplyr::filter(full_name %in% posterior_hdi$Parameter)

  n_priors <- nrow(prior_info)

  lapply(seq_len(n_priors),
         \(i) {
           # Extract the distribution parameters for each coefficient
           mu <- prior_info[i,][['args']][[1]][1]
           sigma <- prior_info[i,][['args']][[1]][2]

           # Compute a numeric range for density values
           xvals <- seq(mu-3*sigma, mu+3*sigma, by = sigma/50)

           # Compute the density for the normal distribution, then scale
           # so it fits on the plot
           yvals <- dnorm(xvals, mu, sigma)
           yvals <- yvals/ max(yvals)
           annotate(geom = 'ribbon',
                    x = xvals,
                    ymin = n_priors - i + 1,
                    ymax = (n_priors-i) +1 + yvals,
                    color = 'black',linewidth = .9,
                    fill = 'magenta',
                    alpha = .2)
         })
}
