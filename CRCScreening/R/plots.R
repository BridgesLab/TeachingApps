# Plots ----------------------------------------------------------------------
#
# University of Michigan brand colors (brand.umich.edu/design-resources/colors).
# Assignments were checked with simulated colour-vision deficiency
# (colorspace::deutan/protan/tritan). Outcomes also differ by shape and size,
# and the "your profile" curve is dashed, so nothing relies on colour alone.
# Maize is used only for filled areas, never for lines or text on white,
# because its contrast against white is low.

UM_COLORS <- c(
  blue = "#00274C", maize = "#FFCB05",
  tappan_red = "#9A3324", ross_orange = "#D86018", rackham_green = "#75988D",
  wave_field_green = "#A5A508", taubman_teal = "#00B2A9", arboretum_blue = "#2F65A7",
  a2_amethyst = "#702082", matthaei_violet = "#575294", peony_pink = "#E01F7C",
  umma_tan = "#CFC096", angell_hall_ash = "#989C97", law_quad_stone = "#655A52",
  puma_black = "#131516"
)

OUTCOME_COLORS <- c(
  tp = UM_COLORS[["blue"]], fn = UM_COLORS[["ross_orange"]],
  fp = UM_COLORS[["maize"]], tn = "#D9D9D9"
)
OUTCOME_SHAPES <- c(tp = 16, fn = 17, fp = 15, tn = 16)

GROUP_COLORS <- c(
  average = UM_COLORS[["blue"]], fdr = UM_COLORS[["ross_orange"]],
  prs_top10 = UM_COLORS[["taubman_teal"]], lynch_MLH1 = UM_COLORS[["tappan_red"]],
  lynch_MSH2 = UM_COLORS[["arboretum_blue"]], lynch_MSH6 = UM_COLORS[["a2_amethyst"]],
  lynch_PMS2 = UM_COLORS[["peony_pink"]], custom = UM_COLORS[["law_quad_stone"]]
)

#' Percent labels that stay readable for very small probabilities
label_small_pct <- function(x) {
  ifelse(is.na(x), NA, paste0(formatC(x * 100, format = "fg", digits = 2), "%"))
}

theme_app <- function(base_size = 15) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot"
    )
}

outcome_labels <- function(target) {
  disease <- if (target == "crc") "cancer" else "cancer/adv. adenoma"
  c(tp = paste0("True positive (", disease, ", test +)"),
    fn = paste0("False negative (", disease, ", test −)"),
    fp = paste0("False positive (no ", sub("/.*", "", disease), ", test +)"),
    tn = "True negative")
}

#' Grid layout of n people, cancers first so the rare ones sit top-left
icon_array_data <- function(counts, n = 10000, ncol = 100) {
  k <- round_counts(counts[c("tp", "fn", "fp", "tn")], n)
  tibble::tibble(
    id = seq_len(n),
    x = (id - 1) %% ncol,
    y = -((id - 1) %/% ncol),
    outcome = factor(rep(names(k), k), levels = c("tp", "fn", "fp", "tn"))
  )
}

plot_icon_array <- function(counts, target = "crc", n = 10000, narrow = FALSE) {
  df <- icon_array_data(counts, n)
  big <- df$outcome %in% c("tp", "fn")
  n_big <- sum(big)

  # the legend is drawn in HTML (icon_legend_ui) so it can wrap on phones
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = outcome, shape = outcome)) +
    ggplot2::geom_point(data = df[!big, ], size = if (narrow) 0.35 else 0.9) +
    ggplot2::geom_point(data = df[big, ], size = if (narrow) 3 else 5, stroke = 1)
  # when cancers are rare, point them out so nobody has to hunt for one dot
  if (n_big > 0 && n_big <= 30) {
    last <- df[big, ][n_big, ]
    p <- p + ggplot2::annotate(
      "label", x = last$x + 3, y = last$y + 3, hjust = 0, vjust = 0,
      size = if (narrow) 3.2 else 4.5,
      label = paste0("\u2190 ", n_big, if (n_big == 1) " person" else " people", " with ",
                     if (target == "crc") "cancer" else "the target condition"),
      fill = "white", linewidth = 0.3)
  }
  p +
    ggplot2::scale_colour_manual(values = OUTCOME_COLORS, drop = FALSE, guide = "none") +
    ggplot2::scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-0.5, 99.5)) +
    ggplot2::scale_y_continuous(limits = c(-99.5, 8)) +
    ggplot2::coord_equal(expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(4, 4, 4, 4))
}

#' HTML legend for the icon array, with counts
icon_legend_ui <- function(counts, target = "crc", n = 10000) {
  k <- round_counts(counts[c("tp", "fn", "fp", "tn")], n)
  labs <- outcome_labels(target)
  glyph <- c(tp = "\u25CF", fn = "\u25B2", fp = "\u25A0", tn = "\u25CF")
  htmltools::tags$ul(
    class = "list-unstyled mb-0 small",
    lapply(names(labs), function(o) {
      htmltools::tags$li(
        htmltools::tags$span(glyph[[o]], style = paste0("color:", OUTCOME_COLORS[[o]],
                                                      "; font-size:1.3em; margin-right:.4em;"),
                             `aria-hidden` = "true"),
        paste0(labs[[o]], ": "), htmltools::tags$strong(scales::comma(k[[o]]))
      )
    })
  )
}

#' Pre-test -> post-test probability bars
prob_ladder_data <- function(res, test_label, sequential) {
  df <- tibble::tibble(
    stage = c("Before the test", paste0("After a NEGATIVE ", test_label),
              paste0("After a POSITIVE ", test_label)),
    prob = c(res$prev_target, res$post_neg, res$ppv),
    kind = c("pre", "neg", "pos")
  )
  if (sequential && !is.na(res$ppv_seq)) {
    df <- dplyr::bind_rows(df, tibble::tibble(
      stage = "After POSITIVE test, then positive colonoscopy + biopsy",
      prob = res$ppv_seq, kind = "seq"))
  }
  df$stage <- paste0(df$stage, ": ", fmt_prob(df$prob))
  df$stage <- factor(df$stage, levels = df$stage)
  df
}

#' One bar per panel with the label above it, so long labels wrap on phones
plot_prob_ladder <- function(df, log_scale = TRUE, narrow = FALSE) {
  fills <- c(pre = UM_COLORS[["angell_hall_ash"]], neg = UM_COLORS[["arboretum_blue"]],
             pos = UM_COLORS[["ross_orange"]], seq = UM_COLORS[["blue"]])
  lo <- if (log_scale) 1e-6 else 0
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lo, xmax = pmax(prob, lo), ymin = 0, ymax = 1, fill = kind)) +
    ggplot2::facet_wrap(~stage, ncol = 1,
                        labeller = ggplot2::label_wrap_gen(width = if (narrow) 32 else 60))
  p <- if (log_scale) {
    p + ggplot2::scale_x_log10(limits = c(lo, 1), breaks = 10^c(-5, -3, -1), labels = label_small_pct)
  } else {
    p + ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent)
  }
  p +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::labs(x = "Probability", y = NULL) +
    theme_app(if (narrow) 12 else 14) +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())
}

plot_ppv_curves <- function(curves, current, log_scale = TRUE, narrow = FALSE) {
  floor <- 1e-5
  curves <- curves |>
    dplyr::mutate(colour_key = ifelse(profile == "Your profile (with lifestyle)", "custom", group),
                  highlight = colour_key == current$colour_key,
                  dplyr::across(dplyr::any_of(c("ppv", "lower", "upper")), \(x) pmax(x, floor)))
  pal <- GROUP_COLORS
  lab <- c(setNames(names(GROUP_CHOICES), GROUP_CHOICES), custom = "Your profile (with lifestyle)")
  present <- intersect(names(pal), curves$colour_key)

  p <- ggplot2::ggplot(curves, ggplot2::aes(age, ppv, colour = colour_key, group = profile))
  if ("lower" %in% names(curves)) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = colour_key),
                                  alpha = 0.12, colour = NA)
  }
  p <- p +
    ggplot2::geom_line(ggplot2::aes(linewidth = highlight,
                                    linetype = colour_key == "custom")) +
    ggplot2::geom_point(data = current, ggplot2::aes(age, ppv), inherit.aes = FALSE,
                        size = 4, shape = 21, fill = "white", stroke = 1.5) +
    ggplot2::scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.8), guide = "none") +
    ggplot2::scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
    ggplot2::scale_colour_manual(values = pal[present], breaks = present, labels = lab[present], name = NULL) +
    ggplot2::scale_fill_manual(values = pal[present], breaks = present, labels = lab[present], name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      ncol = if (narrow) 1 else 2,
      override.aes = list(linetype = ifelse(present == "custom", "22", "solid"))), fill = "none") +
    ggplot2::labs(x = "Age", y = "PPV (chance of cancer if positive)") +
    theme_app(if (narrow) 11 else 15)
  if (log_scale) {
    p + ggplot2::scale_y_log10(limits = c(floor, 1), breaks = 10^(-5:0), labels = label_small_pct)
  } else {
    p + ggplot2::scale_y_continuous(labels = scales::percent)
  }
}

plot_ppv_posterior <- function(draws, point) {
  s <- summarise_draws(draws$ppv)
  ggplot2::ggplot(draws, ggplot2::aes(ppv)) +
    ggplot2::geom_histogram(bins = 40, fill = UM_COLORS[["arboretum_blue"]], colour = "white") +
    ggplot2::geom_vline(xintercept = c(s$lower, s$upper), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = point, linewidth = 1.2, colour = UM_COLORS[["ross_orange"]]) +
    ggplot2::scale_x_continuous(labels = label_small_pct) +
    ggplot2::labs(x = "Positive predictive value", y = "Simulations") +
    theme_app()
}
