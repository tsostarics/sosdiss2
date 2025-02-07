# It is not worth my time to accommodate all of these as strings right now
utils::globalVariables(
  c(
    "n",
    ".data",
    "exp_trial_number",
    "participant",
    "critical_cond",
    "polar_cond",
    "frameRate",
    "original_item",
    "tune",
    "condition",
    "prime_audio",
    "item",
    "target_word",
    "lexdec.response",
    "lexdec.correct",
    "lexdec.buttonbox.keys",
    "lexdec.buttonbox.rt",
    "lexdec.buttonbox.rd",
    "audio.stopped",
    "lexdec_text.started",
    "block_number",
    "matches",
    "rt",
    "rd",
    "si_rt",
    "si_rd",
    "logrt",
    "rt_max",
    "rt_min",
    "Word",
    "Log_Freq_HAL",
    "log_freq",
    "higher",
    "lower",
    "ratio",
    "log_ratio",
    "n_letters",
    "trial_number",
 ".ci", '.epred', '.width', 'after_stat', 'cond_mn',
 "exceeds_pd", 'geom_point', 'geom_rect', 'geom_text', 'label', 'labeller', 'med',
 'tune_i', 'val',
 "width", 'x', 'xcenter', 'xmax', 'xmin', 'y', 'ymax', "ymin", "ypos"

  )
)

.onLoad <- function(libname, pkgname){
  options(SOSDISS2_SKIP_PLOT_SAVE = FALSE)
  options(SOSDISS2_DPI = 300)
}
