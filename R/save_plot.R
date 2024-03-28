#' Save plot helper
#'
#' Saves a given plot as an svg, pdf, and png file with given parameters.
#' Recommended to use `ggview` to settle on the appropriate width, height, and
#' scale, then pass the parameters to this function
#'
#' Assumes a `Figures/` directory in the current working directory
#' @param p Plot
#' @param filename String, filename to use, no file extension
#' @param dpi integer, dpi to use for png file
#' @param ... Additional names parameters to pass
#'
#' @return Invisibly returns the paths to the saved plots
#' @export
save_plot <- function(p, filename, dpi = 1200, ...) {
  plot_path <- \(ext) file.path(here::here("Figures/"), paste0(filename, ".", ext))

  # Save SVG file
  ggplot2::ggsave(plot = p, filename = plot_path("svg"),
                  device = grDevices::svg,
                  ...)
  # Save PDF file
  ggplot2::ggsave(plot = p, filename = plot_path("pdf"),
                  device = grDevices::cairo_pdf,
                  ...)
  # grDevices::dev.off() # Closes devices so we can resume plotting in main session

  # Save PNG file
  rlang::inject(
    ggplot2::ggsave(plot = p, filename = plot_path("png"),
                    device = grDevices::png, dpi = dpi,
                    # We need to use the cairo png device, which requires
                    # passing type = 'cairo' to the graphics device via ...
                    ...=!!!c(type = "cairo", rlang::dots_list(...))))

  invisible(vapply(c('svg','pdf','png'), plot_path, "char"))
}


#' Save posterior predictive check plots
#'
#' @param mdl Model fit with `brm`
#' @param filename Filename to use for files
#' @param show Logical, whether to return the plot object
#' @param pp_options List of named arguments to pass to `brms::ppcheck` such as
#' type or group
#' @param ... Options to pass to save_plot
#'
#' @return plot from pp_check if show is TRUE, otherwise nothing
#' @export
save_ppcheck <- function(mdl,
                         filename,
                         show=FALSE,
                         pp_options = list(),
                         ...) {
  p <- rlang::inject(brms::pp_check(mdl, ndraws = 100, ...=!!!pp_options)) + theme(legend.position = 'top')

  save_plot(p, filename, ...)
  if (show)
    p
}
