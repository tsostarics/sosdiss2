#' Filter lexical decision data
#'
#' Filters experiment data to only a subset of important columns
#'
#' @param subj_data Individual subject data frame
#'
#' @return Filtered dataframe with a subset of renamed columns
#' @export
#'
#' @examples
#' \dontrun{
#' map_dfr(subj_data, filter_exp_data)
#' }
#'
#' @importFrom dplyr across all_of mutate
filter_exp_data <- function(subj_data) {
  # If this is a dual task experiment, add in the SI related columns
  si_columns <- character(0)
  is_dualtask <- 'si.buttonbox.keys' %in% colnames(subj_data)
  if (is_dualtask)
    si_columns <- c(
      si_response = 'si.response',
      si_rt = 'si.buttonbox.rt',
      si_rd = 'si.buttonbox.rd')

  cleaned_data <-
    subj_data |>
    dplyr::filter(!is.na(exp_trial_number)) |>  # Remove practice trials
    dplyr::select(participant,
                  critical_cond,
                  polar_cond,
                  frame_rate = frameRate,
                  original_item,
                  tune,
                  condition,
                  audio_file = prime_audio,
                  item,
                  target_word,
                  response = lexdec.response,
                  is_correct = lexdec.correct,
                  button_pressed = lexdec.buttonbox.keys,
                  rt = lexdec.buttonbox.rt,
                  rd = lexdec.buttonbox.rd,
                  all_of(si_columns),
                  audio.stopped,
                  lexdec_text.started,
                  block_number,
                  trial_number = exp_trial_number) |>
    dplyr::mutate(soa_latency = lexdec_text.started - audio.stopped,
                  # While very rare, if an RT or RD measurement is negative,
                  # this is a measurement error and needs to be removed
                  across(matches("r[td]$"), ~ifelse(. < 0, NA_real_, .)),
                  logrt = log(rt),
                  logrd = log(rd),
                  frame_rate = as.numeric(frame_rate)) |>
    dplyr::select(-audio.stopped, -lexdec_text.started)

  # Compute log rt and rd for the SI task if this is a dual task
  if (is_dualtask)
    cleaned_data <-
    mutate(cleaned_data,
           si_logrt = log(si_rt),
           si_logrd = log(si_rd))

  cleaned_data
}
