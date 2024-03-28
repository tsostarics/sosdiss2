#' Apply initial rt/rd transformations
#'
#' Flags trials as an outlier if they are 3*IQR below Q1/above Q3 or if the RT
#' is above a given threshold based on visualizing the overall distribution.
#' Trials where the RD is too fast is treated as a motor error.
#'
#' @param data Experiment data with multiple participants' raw data
#' @param by Column name for the participant IDs, usually participant or subj_id
#' @param rt_threshold Numeric, default 1500, upper bound of RTs. Check overall
#' distribution first.
#' @param rd_threshold Numeric, default 40, lower bound of RDs. Check overall
#' distribution first.
#'
#' @return Data with additional columns.
#' @export
#'
#' @importFrom dplyr ungroup
#' @importFrom stats IQR median quantile
flag_trial_outliers <- function(data,
                                by = 'participant',
                                rt_threshold = 1500,
                                rd_threshold = 40) {
  dplyr::mutate(ungroup(data),
                rt_max = exp(quantile(logrt, .75) + 3 *IQR(logrt)),
                rt_min = exp(quantile(logrt, .25) - 3 * IQR(logrt)),
                is_outlier = rt > rt_max | rt > rt_threshold | rd < rd_threshold | rt < rt_min,
                .by = all_of(by))
}

#' Apply centering and transformations to covariates
#'
#' Given experiment data that has been filtered for outliers, center various
#' measures.
#'
#' @param filtered_data Data with outliers filtered out already
#'
#' @return filtered_data with modified columns
#' @export
wrangle_covariates <- function(filtered_data) {
  dplyr::mutate(
    ungroup(filtered_data),
    n_letters = stringr::str_length(target_word),
    n_letters = n_letters - mean(stringr::str_length(unique(target_word))),
    trial_number = trial_number - median(trial_number),
    original_item = factor(paste0(original_item, condition)),
    block = block_number - 3.5)
}

#' Join in external data to experiment data
#'
#' Merges in lexical measures and computes frequency ratios for each scale
#'
#' @param data Experimental data
#' @param metrics Elexicon metrics, see `load_metrics`
#'
#' @return data with lexical metrics
#' @export
join_metrics <- function(data, metrics) {
  ratios <-
    data |>
    dplyr::filter(condition %in% c('lower', 'higher')) |>
    dplyr::group_by(item, target_word, condition) |>
    dplyr::summarize() |>
    dplyr::left_join(dplyr::select(metrics,
                                   target_word = Word,
                                   log_freq = Log_Freq_HAL)) |>
    tidyr::pivot_wider(names_from = condition,
                       values_from = log_freq) |>
    dplyr::rename(lower_alt_freq = higher,
                  higher_alt_freq = lower) |>
    dplyr::mutate(alternative_status = ifelse(is.na(higher_alt_freq), "lower", "higher")) |>
    dplyr::group_by(item) |>
    dplyr::mutate(higher_alt_freq = higher_alt_freq[!is.na(higher_alt_freq)],
                  lower_alt_freq = lower_alt_freq[!is.na(lower_alt_freq)],
                  # I want the frequency ratio of the target over the prime.
                  # If I am the HIGHER alternative, then I am the target for the
                  # LOWER condition, so my frequency should be over the LOWER
                  # alternative's frequency.
                  target_prime_ratio = ifelse(alternative_status == "higher",
                                 higher_alt_freq/lower_alt_freq,
                                 lower_alt_freq/higher_alt_freq),
                  log_target_prime_ratio = log(target_prime_ratio))

  data |>
    dplyr::left_join(metrics, by = c('target_word' = "Word")) |>
    dplyr::left_join(dplyr::select(ratios,
                                   target_word,
                                   item,
                                   log_target_prime_ratio),
                     by = c('target_word', 'item'))
}
