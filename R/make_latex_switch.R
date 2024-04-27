#' Convert model to latex switch command
#'
#' Given a mixed model (either `brmsfit` or `mermod` object), extract the fixed
#' effect coefficient estimates and format the results as an inline statistic.
#' The output of this is a latex command definition that returns the inline
#' statistic given a coefficient name. This should be written to a .tex document
#' and imported into your latex project.
#'
#' Note that this is a wrapper that processes the model coefficients for you.
#' If you need more control over the model coefficients, or want to use only
#' a subset of the coefficients, you should wrangle that dataframe and then use
#' `make_latex_switch()`.
#'
#' @param model `brmsfit` or `mermod` model object
#' @param macroname Name to use for the latex command
#' @param fstring Formatted glue string to use for the inline citation. Use
#' column names that appear from the output of `broom.mixed::tidy()`
#' @param roundto Number of digits to round the numeric results to, defaults
#' to 2 decimal places.
#' @param remove_parens Whether to remove parentheses from coefficient names,
#' such as in `(Intercept)`
#' @param add_found_boolean Whether to define a new boolean value `found` for the
#' latex command. This should be set to TRUE for the first switch command created,
#' but all following commands should use FALSE. Alternatively, you can create
#' the boolean globally in your LaTeX preamble and just always use FALSE with
#' this function.
#'
#' @return A latex command with the given name as a character vector for each
#' line of the command. The vector is returned invisibly, but the entire command
#' is printed via `cat()` so it can be easily copy and pasted into a document
#' as needed.
#' @export
model_to_latex_switch <- function(model,
                                  macroname = "statvalue",
                                  fstring = r"($(\hat\beta = {estimate}, CI=[{conf.low},{conf.high}])$)",
                                  roundto = 2,
                                  remove_parens = TRUE,
                                  add_found_boolean = TRUE) {
  model |>
    process_model_coef_df(roundto, remove_parens) |>
    make_latex_switch(macroname, fstring, remove_parens, add_found_boolean)
}

#' Process model coefficients
#'
#' A helper to process model coefficients in a consistent manner for writing
#' latex switch commands.
#'
#' @param model `brmsfit` or `mermod` model object
#' @param roundto Number of digits to round the numeric results to, defaults
#' to 2 decimal places.
#' @param remove_parens Whether to remove parentheses from coefficient names,
#' such as in `(Intercept)`
#'
#' @return Dataframe of model fixed effect coefficients using `broom.mixed::tidy()`
#'
#' @export
process_model_coef_df <- function(model, roundto = 2, remove_parens=TRUE) {
  # Process the model coefficients, rounding the numeric values as needed
  mdl_coef_df <- suppressWarnings(broom.mixed::tidy(model, effects = "fixed"))
  mdl_coef_df[['estimate']] <- round(mdl_coef_df[['estimate']], roundto)

  # If the confidence intervals aren't returned for non-bayesian models, just
  # make sure to create them.
  if (!"conf.high" %in% colnames(mdl_coef_df)) {
    mdl_coef_df[['conf.low']] <- mdl_coef_df[['estimate']] - 1.96 * mdl_coef_df[['std.error']]
    mdl_coef_df[['conf.high']] <- mdl_coef_df[['estimate']] + 1.96 * mdl_coef_df[['std.error']]
  }

  mdl_coef_df[['conf.low']] <- round(mdl_coef_df[['conf.low']], roundto)
  mdl_coef_df[['conf.high']] <- round(mdl_coef_df[['conf.high']], roundto)

  # The : character isn't allowed, so we'll switch to i for interaction
  mdl_coef_df[['term']] <- casefold(gsub(":", "i", mdl_coef_df[['term']]))


  # If we want to remove parentheses from, eg, the intercept term, do so
  if (remove_parens)
    mdl_coef_df[['term']] <- gsub("[)(]", "", mdl_coef_df[['term']])

  mdl_coef_df
}

#' Create a latex switch command
#'
#' Given a dataframe, create a latex switch command.
#'
#' @param model_coef_df Dataframe containing model coefficient information
#' @param macroname Name to use for the latex command
#' @param fstring Formatted glue string to use for the inline citation. Use
#' column names that appear from the output of `broom.mixed::tidy()`
#' @param remove_parens Whether to remove parentheses from coefficient names,
#' such as in `(Intercept)`
#' @param add_found_boolean Whether to define a new boolean value `found` for the
#' latex command. This should be set to TRUE for the first switch command created,
#' but all following commands should use FALSE. Alternatively, you can create
#' the boolean globally in your LaTeX preamble and just always use FALSE with
#' this function.
#'
#' @return A latex command with the given name as a character vector for each
#' line of the command. The vector is returned invisibly, but the entire command
#' is printed via `cat()` so it can be easily copy and pasted into a document
#' as needed.
#'
#' @export
make_latex_switch <- function(model_coef_df,
                              macroname = "statvalue",
                              fstring = r"($(\hat\beta = {estimate}, CI=[{conf.low},{conf.high}])$)",
                              remove_parens = TRUE,
                              add_found_boolean = TRUE) {
  if(!grepl("^[a-zA-Z]+$", macroname))
    stop(paste0("Invalid macroname '", macroname,"'. Macro names should only contain letters."))

  stopifnot(length(macroname) == 1L)

  # Set up the start of the command with the expl3 lines
  starting_lines <- c(r"(\ExplSyntaxOn)")

  # Base case is handled by a boolean called "found", if we have already defined
  # this elsewhere then we can omit it as needed
  if (add_found_boolean)
    starting_lines <- c(starting_lines, r"(\newboolean{found})")

  starting_lines <-
    c(
      starting_lines,
      r"(\setboolean{found}{false})",
      paste0(r"(\NewDocumentCommand \)", macroname, r"({ m })"), # {m} see https://tex.stackexchange.com/questions/644668/latex3-verbatim-like-command-illegal-in-argument
      r"(  {)",
      r"(    \str_case_e:nn { \str_foldcase:e { #1 } })",
      r"(      {)")


  # Take the formatted string from the user and embed it within the syntax
  # needed for the conditional statements
  fstring <- paste0(
    "        {{ {term} }} {{ \\setboolean{{found}}{{true}} ", fstring, " }}")

  # Take the full formatted string and inject the model values
  mdl_lines <- glue::glue(fstring, .envir = model_coef_df)

  # Add together all the lines, along with the ending parts to close off the
  # command definition
  all_lines <-
    c(starting_lines,
      mdl_lines,
      r"(      })",
      r"(    \ifthenelse{\boolean{found}}{}{{\color{red} \large ERROR}})",
      r"(    \setboolean{found}{false})",
      r"(  })",
      r"(\cs_generate_variant:Nn \str_foldcase:n { e })",
      r"(\ExplSyntaxOff)") |>
    paste0("\n") # add newlines for formatting

  # Print the new expression with the newlines in a copy-pasteable format
  cat(all_lines)

  # Return the lines as a character vector in case the user wants to hold on
  # to them (which is reasonable)
  invisible(all_lines)
}

