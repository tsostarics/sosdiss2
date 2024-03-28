#' Cross tabulate list assignments
#'
#' Given experimental data with participants assigned to different critical
#' and polar conditions, cross tabulate the number of participants assigned
#' to each list
#'
#' @param data Experimental data of trials for each participant
#' @param participant Column name containing the participant IDs, usually either
#' participant or subj_id
#' @param critical Column name containing the critical condition list numbers,
#' usually critical_cond
#' @param polar Column name containing the polar condition list numbers,
#' usually polar_cond
#'
#' @return List of dataframes
#' @export
count_lists <- function(data,
                        participant = 'participant',
                        critical = 'critical_cond',
                        polar = 'polar_cond') {
  by_critical <- c(participant, critical)
  by_polar <- c(participant, polar)

  lapply(list(by_critical, by_polar),
         \(l) {
           data |>
             dplyr::summarize(.by = all_of(l)) |>
             dplyr::summarize(n = n(),
                       .by = all_of(l[-1])) |>
             dplyr::arrange(.data[[l[-1]]])
         }) |>
    `names<-`(c('critical', 'polar'))
}
