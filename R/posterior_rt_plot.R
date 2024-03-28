#' Title
#'
#' @param FACET_CONDITION String, which facet to draw the legend in
#' @param RECT_TOP Top y value for the legend rectangle
#' @param RECT_HEIGHT Height of the legend rectangle
#' @param CI_HEIGHT Height of the credible interval bars
#' @param CI_LEFT Left edge of the credible interval bars
#'
#' @return A list of geoms for drawing a legend inside a facet
#' @export
#'
#' @importFrom ggplot2 aes labeller element_line element_blank geom_rect geom_point geom_line position_nudge theme
#' @importFrom brms variables
make_draws_legend <- function(
    FACET_CONDITION = "lower",
    RECT_TOP = 559,
    RECT_HEIGHT = 16,
    CI_HEIGHT = 5,
    CI_LEFT = 1.5
){
  # Calculate dimensions for legend elements
  RECT_BOTTOM <- RECT_TOP - RECT_HEIGHT
  CI_BOTTOM <- RECT_BOTTOM + 1
  CI_TOP <- CI_BOTTOM + CI_HEIGHT
  BOTTOM_TEXT <- CI_BOTTOM + CI_HEIGHT / 2

  # Make dataframe for credible interval bars
  ci_bars <-
    data.frame(width = 2.4,
               xcenter = 2,
               ymin = CI_BOTTOM,
               ymax = CI_TOP) |>
    dplyr::reframe(.ci = c(.5, .86, .95),
            xmin = xcenter - width*c(.5, .86, .95),
            xmax = xcenter + width*c(.5, .86, .95),
            ymin, ymax) |>
    dplyr::mutate(condition = FACET_CONDITION) |>
    dplyr::arrange(dplyr::desc(.ci)) # Ensures bars are layered correctly

  ci_bars_geoms <-
    list(
      # Draw the bars one on top of the other
      geom_rect(data = ci_bars,
                aes(xmin = CI_LEFT, ymin = ymin,
                    xmax = xmax, ymax = ymax,
                    fill = factor(.ci)),
                color = NA,
                inherit.aes = FALSE) ,
      # use the widest bar to draw an outline around the whole CI bar
      geom_rect(data = dplyr::filter(ci_bars, .ci== .95),
                aes(xmin = CI_LEFT, ymin = ymin,
                    xmax = xmax, ymax = ymax,
                    fill = factor(.ci)),
                inherit.aes = FALSE,
                fill = NA,
                color = "#380202") ,
      ggnewscale::new_scale("fill") ,
      # Do the same thing for the gray bars
      ggplot2::scale_fill_brewer(palette = 'Greys',direction = -1),
      geom_rect(data = ci_bars,
                aes(xmin = CI_LEFT, ymin = ymin+5,
                    xmax = xmax, ymax = ymax+5,
                    fill = factor(.ci)),
                color = NA,
                inherit.aes = FALSE) ,
      geom_rect(data = dplyr::filter(ci_bars, .ci== .95),
                aes(xmin = CI_LEFT, ymin = ymin+5,
                    xmax = xmax, ymax = ymax+5,
                    fill = factor(.ci)),
                inherit.aes = FALSE,
                fill = NA,
                color ='gray5'))

  # Add the text and point annotations
  legend_annotations <-
    list(
      annotate_facet(condition = FACET_CONDITION,
                     x = CI_LEFT - .1, y = BOTTOM_TEXT+CI_HEIGHT*1.75, label = "CI Width",
                     fixed = list(color='black',
                                  hjust=1,
                                  vjust = 0),
                     aes = aes(x = x, y = y, label=label, group = condition)),
      annotate_facet(condition = FACET_CONDITION,
                     x = CI_LEFT-.15, y = BOTTOM_TEXT+CI_HEIGHT, label = "pd < .95",
                     fixed = list(color='black',
                                  hjust=1,
                                  vjust = .5),
                     aes = aes(x = x, y = y, label=label, group = condition)),
      annotate_facet(condition = FACET_CONDITION,
                     x = CI_LEFT-.15, y = BOTTOM_TEXT, label = "pd > .95",
                     fixed = list(color='firebrick4',
                                  hjust=1,
                                  vjust = .5),
                     aes = aes(x = x, y = y, label=label, group = condition)),
      annotate_facet(condition = FACET_CONDITION,
                     x = c(1.5,3.2,4.05),
                     y = BOTTOM_TEXT+CI_HEIGHT*1.75, label = c('.5','.86','.95'),
                     fixed = list(color='black',
                                  hjust=0,
                                  vjust = 0),
                     aes = aes(x = x, y = y, label=label, group = condition)),
      annotate_facet(condition = FACET_CONDITION,
                     x = CI_LEFT, y = BOTTOM_TEXT+CI_HEIGHT,
                     fixed = list(color = 'black', size = 3),
                     geom = geom_point,
                     aes = aes(x = x, y = y)),
      annotate_facet(condition = FACET_CONDITION,
                     x = CI_LEFT, y = BOTTOM_TEXT, size = 1,
                     fixed = list(shape = 23, color = 'black', fill = '#fee0d2',
                                  size = 3),
                     geom = geom_point,
                     aes = aes(x = x, y = y))
    )

  # Make a background to go behind the legend
  legend_bg <-
    list(annotate_facet(geom = geom_rect,
                        condition = FACET_CONDITION,
                        xmin = .4, xmax = 4.5,
                        ymin = RECT_BOTTOM, ymax = RECT_TOP,
                        aes = ggplot2::aes(xmin = xmin,
                                           xmax = xmax,
                                           ymin = ymin,
                                           ymax = ymax,
                                           group = condition),
                        fixed = list(fill = 'white', color = 'gray20')))

  list('legend_bg' = legend_bg,
       'ci_bars_geoms' = ci_bars_geoms,
       'legend_annotations' = legend_annotations)
}

#' Add annotation to a single facet
#'
#' Usually adding an annotation will draw something to every facet.
#' This helper constructs a dataframe out of the annotation parameters such that
#' the annotation is drawn in only a single facet.
#'
#' @param geom Geometry function for the annotation
#' @param condition Which facet to draw in
#' @param aes aesthetic mapping
#' @param fixed Fixed aesthetics to use
#' @param ... Values to annotate with, passed to a dataframe
#'
#' @return List of annotation geometries
annotate_facet <- function(geom = geom_text,
                           condition,
                           aes,
                           fixed = list(color = 'red'),
                           ...) {
  annotation_df <- as.data.frame(rlang::dots_list(...))
  annotation_df$condition <-  condition

  c(list(
    geom,
    data = annotation_df,
    mapping = aes,
    inherit.aes = FALSE
  ), fixed) |>
    as.call() |>
    eval()

}

#' Plot posterior draws
#'
#' Plots the posterior expectation draws for the higher and lower conditions,
#' colored by which conditions have pd > .95.
#'
#' @param pred_draws Posterior expectation draws (from add_epred_draws)
#' @param intercept_lines Dataframe of intercept values to draw as lines
#' @param scale Scaling factor for text, should match scale argument in `save_plot`
#' @param ... Not used
#' @param ylim plot y limits, passed to coord_cartesian
#' @param facet_labels Named character to label facets, names must equal the
#' condition names. Defaults to the 2 condition case.
#'
#' @return plot
#' @export
plot_posterior_rt_draws <- function(pred_draws, intercept_lines,
                                    ylim = c(470,559),
                                    scale = 1.2,
                                    facet_labels = c('lower'  = "Hear tough, see impossible",
                                                     'higher' = 'Hear impossible, see tough'),
                                    ...) {
  # legend_geoms <- make_draws_legend(RECT_TOP = RECT_TOP, ...)
  pct_vals <- make_percent_labels(pred_draws)

  pred_draws |>
    ggplot2::ggplot(aes(x = tune_i, y = (.epred), group = tune)) +
    # Plot the condition averages as horizontal dashed lines
    ggplot2::geom_line(data = dplyr::reframe(intercept_lines, y = y[1],x=0:8, .by = 'condition'),
                       aes(x = x, y=y),
                       linetype = 'dashed',
                       inherit.aes = FALSE) +
    # Plot the posterior slabs for each condition where pd < .95
    ggdist::stat_slabinterval(data= dplyr::filter(pred_draws, !exceeds_pd),
                              aes(fill = factor(after_stat(.width)),
                                  x = tune_i,
                                  y = (.epred), group = tune),
                              point_interval = "mean_hdi",
                              shape = 1,
                              .width = c(.5,.86,.95)) +
    ggplot2::scale_fill_brewer(palette = 'Greys', direction = -1) +
    # Plot the posterior slabs for each condition where pd > .95
    ggnewscale::new_scale_fill() +
    ggdist::stat_slabinterval(data= dplyr::filter(pred_draws, exceeds_pd),
                              aes(fill = factor(after_stat(.width)),
                                  x = tune_i,
                                  y = (.epred), group = tune),
                              point_interval = "mean_hdi",
                              height = 1,
                              scale = .5,
                              color = "#380202",
                              point_fill = "#fee0d2",
                              fatten_point = 2.25,
                              shape = 23,
                              .width = c(.5,.86,.95)) +
    ggplot2::scale_fill_brewer(palette = 'Reds',direction = -1) +
    # Label each distribution with the percentage difference from the condition mean
    ggplot2::geom_text(data = pct_vals,
                       aes(x = tune_i,
                           y = ypos,
                           label = paste0(label,"%")),
                       position = position_nudge(x=.41,y=1),
                       angle = -15,
                       color = 'white') +
    ggplot2::facet_wrap(~condition,
                        labeller = labeller(.cols = facet_labels)) +
    ggplot2::theme_bw(base_size = 16*scale) +
    ggplot2::scale_x_continuous(breaks = 1:6,
                                labels = levels(pred_draws[['tune']]))+
    ggplot2::theme(panel.grid = element_line(linewidth = .3),
                   panel.grid.minor.x = element_blank(),
                   legend.position = 'none') +
    ggplot2::coord_cartesian(xlim = c(.4,7),
                             ylim = ylim,
                             clip = TRUE,
                             expand = FALSE)+
    ggplot2::xlab("Tune") +
    ggplot2::ylab("Posterior RT Expectation")
}



#' Get speedup/slowdown percentages
#'
#' Given predicted draws, get the mean for each condition and compute the overall
#' percent speedup/slowdown for each tune. So, a value of -2.1 would equal a
#' 2.1% speedup
#'
#' @param pred_draws Output of `add_epred_draws`
#' @param TUNE_ORDER Tune order, only a parameter in the event i need to change
#' it later
#'
#' @return dataframe with summarized percentage values for each tune in each condition
make_percent_labels <- function(pred_draws, TUNE_ORDER = c("lshll", "lhsll", "hll",
                                                           "hlh", "lhslh", "lshlh")) {
  pred_draws |>
    ungroup() |>
    dplyr::mutate(cond_mn = exp(mean(log(.epred))),
                  .by = 'condition') |>
    dplyr::summarize(
      ypos = exp(mean(log(.epred))),
      med = (exp(mean(log(.epred) - log(cond_mn)))),
      val = (med - 1) * 100,
      label = round(val, 2),
      label = ifelse(sign(label) ==-1, as.character(label), paste0("+", label)),
      .by = c('tune', 'condition')) |>
    dplyr::mutate(tune = factor(tune, levels = TUNE_ORDER),
                  tune_i = as.integer(tune))
}
