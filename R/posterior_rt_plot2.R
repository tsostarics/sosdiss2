#' Title
#'
#' @param condition_difference_draws Output of `get_condition_difference_draws()`
#' @param scale Scale factor for plot output, default 1.2
#' @param facet_labels Labels for the condition facets, set NULL to exclude.
#' @param slab_scale Numeric, default 1, change to adjust the scaling of the
#' red slabs. Needed to match the gray ones manually, but the exact value changes
#' depending on the number of reds.
#' @param debug_red Logical, default FALSE, if TRUE will overlay the red slabs
#' over gray slabs. Use to check that the scaling works correctly and adjust
#' slab_scale as needed to fix.
#' @param ... Not used
#'
#' @return a ggplot
#' @export
plot_posterior_rt_draws2 <- function(condition_difference_draws,
                                     scale = 1.2,
                                     slab_scale = 1,
                                     facet_labels = c('lower'  = "Hear tough, see impossible",
                                                      'higher' = 'Hear impossible, see tough'),
                                     debug_red = FALSE,
                                     ...) {
  percent_change_df <- compute_percent_change(condition_difference_draws)

  plot_df <-
    condition_difference_draws |>
    dplyr::left_join(dplyr::select(percent_change_df, condition, tune, past_95))

  condition_difference_draws |>
    ggplot(aes(x = tune, y = difference)) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = 'dashed',
                        color = 'gray10')+
    ggdist::stat_histinterval(data = dplyr::filter(plot_df, !past_95 | debug_red),
                              aes(fill = factor(after_stat(.width))),
                              point_interval = 'mean_qi',
                              .width = c(.5, .89, .95),
                              slab_linewidth = .1,
                              height = 1,
                              scale = .95,
                              trim = FALSE,
                              outline_bars = TRUE) +
    ggplot2::scale_fill_brewer(palette = "Greys", direction = -1) +
    ggnewscale::new_scale_fill() +
    ggdist::stat_histinterval(data = dplyr::filter(plot_df, past_95),

                              aes(fill = factor(after_stat(.width))),
                              point_interval = 'mean_qi',
                              .width = c(.5, .89, .95),
                              slab_linewidth = .1,
                              height = 1,
                              alpha = ifelse(debug_red, .5, 1),
                              scale = slab_scale * .95, # Needed to match the scaling of the gray ones
                              color = "#380202",
                              point_fill = "#fee0d2",
                              fatten_point = 2.25,
                              shape = 23,
                              trim = FALSE,
                              outline_bars = TRUE) +
    scale_thickness_shared()+
    ggplot2::scale_fill_brewer(palette = "Reds", direction = -1) +
    ggplot2::geom_text(data = percent_change_df,
                       aes(x = tune,
                           y = difference,
                           label = pct_label),
                       position = position_nudge(x=.105,y=.002),
                       angle = -15,
                       hjust = 0,
                       # size = 3,
                       color = 'white') +
    ggplot2::facet_grid(~condition,
                        labeller = labeller(.cols = facet_labels)) +
    ggplot2::theme_bw(base_size = 16*scale) +
    ggplot2::coord_cartesian(xlim = c(.5,7.1),
                             # ylim = c(-.05, .05),
                             clip = TRUE,
                             expand = FALSE)+
    scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::theme(panel.grid = element_line(linewidth = .3),
                   panel.grid.minor.x = element_blank(),
                   legend.position = 'none') +
    ggplot2::xlab("Tune") +
    ggplot2::ylab("% Difference\nfrom Condition Mean")
}


#' Title
#'
#' @param model brmsfit object
#' @param dg output of `marginal_data_grid()`
#' @param ndraws Number of draws, set to a small number (~100) to test. Default
#' NULL (all draws)
#' @param TUNE_ORDER Ordering of tunes, standard for project analysis
#'
#' @return Dataframe of the difference in epred logRT measures. Basically
#' the residual RT for each tune, which is interpretable as percent change from
#' some RT baseline. Eg -1.8 = 1.8% speedup.
#' @export
get_condition_difference_draws <- function(model, dg, ndraws = NULL, TUNE_ORDER = c('lshll','lhsll','hll','hlh','lhslh','lshlh')) {
  model_epred_draws <-
    tidybayes::add_epred_draws(newdata = dg,
                               object =  model,
                               re_formula = NA,
                               ndraws = NULL,
                               dpar = TRUE,
                               seed=111) |>
    dplyr::mutate(tune = factor(tune, levels = TUNE_ORDER))

  # Remove the tune predictor so we can just get the condition averages
  dg2 <- dg |>
    dplyr::group_by(dplyr::across(!tidyselect::matches('tune'))) |>
    dplyr::reframe(tune = factor(NA_character_, levels = TUNE_ORDER))

  intercept_draws <-
    tidybayes::add_epred_draws(
      newdata = dg2,
      object =  model,
      re_formula = NA,
      ndraws = NULL,
      dpar = TRUE,
      seed=111)


  model_epred_draws |>
    dplyr::group_by(condition, tune) |>
    dplyr::reframe(difference = mu - intercept_draws[intercept_draws[['condition']] == condition[1],][['mu']])


}


#' Title
#'
#' Helper to wrangle the percent change labels for the posterior rt plot.
#'
#' @param difference_draws Output of `get_condition_difference_draws()`
#'
#' @return Dataframe with information about whether the 95% CrI (mean qi) contains
#' 0 and the associated label for each tune:conditoin
#' @export
compute_percent_change <- function(difference_draws) {

  difference_draws |>
    dplyr::group_by(condition, tune) |>
    tidybayes::mean_qi(difference, .width = .95) |>
    dplyr::mutate(past_95 = !dplyr::between(0, .lower, .upper),
                  .by = c('condition', 'tune'),
                  pct_label = round((exp(difference)-1)*100,2),
                  pct_label = paste0(ifelse(pct_label<0,"","+"), pct_label)) |>
    dplyr::select(-.width, -.point, -.interval)

}
