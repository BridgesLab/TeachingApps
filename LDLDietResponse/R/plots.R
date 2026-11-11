# Plots ----------------------------------------------------------------------
#
# University of Michigan colours. Maize is used only for fills (with blue text
# or outlines), never for lines or text on white, because its contrast is low.

UM <- c(blue = "#00274C", maize = "#FFCB05", arboretum_blue = "#2F65A7",
        tappan_red = "#9A3324", grey = "#7A8CA3", light = "#E8EDF3")

theme_app <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "top", legend.justification = "left",
      axis.text.y = ggplot2::element_text(colour = "black")
    )
}

#' Rows for the waterfall: average terms, average total, knob steps, person total
waterfall_data <- function(pred, mode = "full") {
  comp <- pred$components
  if (mode == "card") comp <- comp[c("sfa", "chol")]
  steps <- pred$knob_steps
  step_labels <- c(B = "B · Absorption", C = "C · Synthesis compensation",
                   `D/E` = "D/E · VLDL flux (hypothesis)", F = "F · LDL receptor / APOE")
  lab <- c(COMPONENT_LABELS[names(comp)], "Average person", step_labels[names(steps)], "This person")
  if (mode == "card") lab[1:2] <- c("A · Saturated fat", "A · Dietary cholesterol")
  val <- c(comp, pred$avg_delta, steps, pred$person_delta)
  kind <- c(rep("avg", length(comp)), "avg_total", rep("knob", length(steps)), "person_total")
  start <- end <- numeric(length(val))
  cum <- 0
  for (i in seq_along(val)) {
    if (kind[i] %in% c("avg_total", "person_total")) {
      start[i] <- 0; end[i] <- val[i]; cum <- val[i]
    } else {
      start[i] <- cum; end[i] <- cum + val[i]; cum <- end[i]
    }
  }
  data.frame(label = lab, value = val, kind = kind, start = start, end = end,
             y = rev(seq_along(val)), stringsAsFactors = FALSE)
}

plot_waterfall <- function(pred, mode = "full") {
  df <- waterfall_data(pred, mode)
  fills <- c(avg = UM[["maize"]], avg_total = UM[["maize"]],
             knob = UM[["arboretum_blue"]], person_total = UM[["blue"]])
  span <- range(c(0, df$start, df$end))
  pad <- diff(span) * 0.18 + 5
  df$lab_x <- ifelse(df$value >= 0, pmax(df$start, df$end), pmin(df$start, df$end))
  df$hjust <- ifelse(df$value >= 0, -0.15, 1.15)
  g <- ggplot2::ggplot(df) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_rect(ggplot2::aes(xmin = pmin(start, end), xmax = pmax(start, end),
                                    ymin = y - 0.38, ymax = y + 0.38, fill = kind),
                       colour = UM[["blue"]], linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(x = lab_x, y = y, label = sprintf("%+.0f", value), hjust = hjust),
                       size = 4.3, fontface = ifelse(grepl("total", df$kind), "bold", "plain")) +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_y_continuous(breaks = df$y, labels = df$label, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(limits = c(span[1] - pad, span[2] + pad)) +
    ggplot2::labs(x = "Change in LDL-C (mg/dL)", y = NULL) +
    theme_app()
  g
}

#' Histogram of LDL-C after the diet change across people (and coefficient draws)
plot_spread <- function(sp, pred, p, show_keto = FALSE) {
  x <- sp$both
  xlim <- range(c(stats::quantile(x, c(0.01, 0.99)), sp$ldl0, pred$avg_after, pred$person_after,
                  if (show_keto) sp$ldl0 * (1 + c(p[["keto_trial_min"]], p[["keto_trial_max"]]) / 100)))
  xlim <- xlim + c(-0.06, 0.06) * max(diff(xlim), 20)
  off <- sum(x < xlim[1] | x > xlim[2])
  df <- data.frame(ldl = x[x >= xlim[1] & x <= xlim[2]])
  ci <- stats::quantile(sp$avg_coef, c(0.025, 0.975), names = FALSE)
  bench <- c(100, 130, 160, 190)
  bench <- bench[bench > xlim[1] & bench < xlim[2]]
  g <- ggplot2::ggplot(df, ggplot2::aes(x = ldl))
  if (show_keto) {
    g <- g + ggplot2::annotate("rect", xmin = sp$ldl0 * (1 + p[["keto_trial_min"]] / 100),
                               xmax = sp$ldl0 * (1 + p[["keto_trial_max"]] / 100),
                               ymin = -Inf, ymax = Inf, fill = UM[["light"]])
  }
  g +
    ggplot2::geom_vline(xintercept = bench, colour = "grey80", linetype = "dotted") +
    ggplot2::geom_histogram(bins = 45, fill = UM[["grey"]], colour = "white", linewidth = 0.2) +
    ggplot2::annotate("rect", xmin = ci[1], xmax = ci[2], ymin = -Inf, ymax = Inf,
                      fill = UM[["maize"]], alpha = 0.35) +
    ggplot2::geom_vline(xintercept = sp$ldl0, colour = "grey40", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = pred$avg_after, colour = "black", linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = pred$person_after, colour = UM[["blue"]], linewidth = 1.2) +
    ggplot2::annotate("label", x = pred$avg_after, y = 0, vjust = -0.3, label = "Average person",
                      fill = UM[["maize"]], colour = UM[["blue"]], size = 3.5) +
    ggplot2::annotate("label", x = pred$person_after, y = 0, vjust = -1.6, label = "This person",
                      fill = UM[["blue"]], colour = "white", size = 3.5) +
    ggplot2::annotate("text", x = sp$ldl0, y = Inf, vjust = 1.3, hjust = 1.08, size = 3.2,
                      colour = "grey30", label = "Before") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(x = "LDL-C after the diet change (mg/dL)", y = "People",
                  caption = paste(c("Yellow: average person's 95% range (coefficient uncertainty)",
                                    if (show_keto) "Grey: individual rises in a real keto trial (Retterstøl 2018)",
                                    if (off > 0) sprintf("%d people off the axis", off)), collapse = "\n")) +
    theme_app() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey92"),
                   plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey30"),
                   plot.caption.position = "plot")
}

plot_fixes <- function(ldl0, base, all) {
  df <- data.frame(
    stage = factor(rep(c("Before", "After the diet change", "With fixes"), 2),
                   levels = c("Before", "After the diet change", "With fixes")),
    who = factor(rep(c("Average person", "This person"), each = 3),
                 levels = c("Average person", "This person")),
    ldl = c(ldl0, base[["average"]], all[["average"]], ldl0, base[["person"]], all[["person"]])
  )
  ggplot2::ggplot(df, ggplot2::aes(x = stage, y = ldl, fill = who)) +
    ggplot2::geom_hline(yintercept = c(100, 130, 160, 190), colour = "grey85") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75,
                      colour = UM[["blue"]], linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = round(ldl)), position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.4, size = 4.2) +
    ggplot2::scale_fill_manual(values = c("Average person" = UM[["maize"]], "This person" = UM[["blue"]])) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "LDL-C (mg/dL)", fill = NULL) +
    theme_app() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
