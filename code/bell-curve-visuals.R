# Two figures for the dartboard derivation in Week 6.
# Keep the calculations local so these figures do not change the lesson's RNG
# state or plotting defaults.

bell_curve_geometry_plot <- function() {
  ink <- "#24352F"
  green <- "#18453B"
  accent <- "#B85C25"
  muted <- "#74827B"
  theta <- seq(0, 2 * pi, length.out = 361)

  plot_theme <- ggplot2::theme_minimal(base_size = 16, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = ink, size = 17),
      plot.subtitle = ggplot2::element_text(color = muted, size = 14),
      axis.title = ggplot2::element_text(color = ink, size = 14),
      axis.text = ggplot2::element_text(color = ink, size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 16, 12, 12),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  target <- expand.grid(
    x = seq(-2.1, 2.1, length.out = 201),
    y = seq(-2.1, 2.1, length.out = 201)
  )
  target$density <- 2^(-(target$x^2 + target$y^2))
  circle <- data.frame(x = sqrt(2) * cos(theta), y = sqrt(2) * sin(theta))
  triangle <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 0))
  corners <- data.frame(x = c(0, 1, 1), y = c(0, 0, 1))

  board <- ggplot2::ggplot(target, ggplot2::aes(x, y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = density)) +
    ggplot2::scale_fill_gradient(low = "#F5F8F6", high = green, guide = "none") +
    ggplot2::geom_hline(yintercept = 0, color = "#BBC6C0", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, color = "#BBC6C0", linewidth = 0.35) +
    ggplot2::geom_path(data = circle, color = accent, linewidth = 0.8,
                       linetype = "dashed") +
    ggplot2::geom_path(data = triangle, color = accent, linewidth = 1.15) +
    ggplot2::geom_point(data = corners, color = accent, fill = "white",
                       shape = 21, stroke = 1.2, size = 3) +
    ggplot2::annotate("label", x = 0.5, y = -0.25, label = "1", size = 4.5,
                      fill = "white", color = ink, linewidth = 0) +
    ggplot2::annotate("label", x = 1.22, y = 0.5, label = "1", size = 4.5,
                      fill = "white", color = ink, linewidth = 0) +
    ggplot2::annotate("label", x = 0.34, y = 0.65, label = "sqrt(2)",
                      parse = TRUE, size = 4.5, fill = "white", color = ink,
                      linewidth = 0) +
    ggplot2::annotate("label", x = 1.28, y = 1.25, label = "(1, 1)",
                      size = 4.2, fill = "white", color = accent, linewidth = 0) +
    ggplot2::scale_x_continuous(breaks = -2:2, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = -2:2, expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Two independent misses",
                  subtitle = expression(1^2 + 1^2 == (sqrt(2))^2),
                  x = "Horizontal error", y = "Vertical error") +
    plot_theme + ggplot2::theme(panel.grid = ggplot2::element_blank())

  slice <- data.frame(x = seq(-2.4, 2.4, length.out = 501))
  slice$density <- 2^(-slice$x^2)
  reference <- data.frame(x = c(0, 1, sqrt(2)), density = c(1, 1/2, 1/4))

  curve <- ggplot2::ggplot(slice, ggplot2::aes(x, density)) +
    ggplot2::geom_area(fill = "#E5EEE8") +
    ggplot2::geom_segment(data = reference[-1, ],
                          ggplot2::aes(xend = x, yend = 0), color = muted,
                          linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_line(color = green, linewidth = 1.25) +
    ggplot2::geom_point(data = reference[1:2, ], color = green, size = 3) +
    ggplot2::geom_point(data = reference[3, ], color = accent, size = 3.5) +
    ggplot2::annotate("text", x = -0.1, y = 1.06, label = "1",
                      color = green, size = 4.7) +
    ggplot2::annotate("text", x = 1.15, y = 0.6, label = "1/2",
                      color = green, size = 4.7) +
    ggplot2::annotate("text", x = 1.68, y = 0.34, label = "1/4",
                      color = accent, size = 4.7) +
    ggplot2::scale_x_continuous(breaks = c(-2, -1, 0, 1, sqrt(2), 2),
                               labels = expression(-2, -1, 0, 1, sqrt(2), 2)) +
    ggplot2::scale_y_continuous(breaks = c(0, 1/4, 1/2, 3/4, 1),
                               labels = c("0", "1/4", "1/2", "3/4", "1")) +
    ggplot2::coord_cartesian(ylim = c(0, 1.12), expand = FALSE) +
    ggplot2::labs(title = "A slice through the center",
                  subtitle = expression(h(x) == 2^{-x^2}),
                  x = "Position along the slice, x",
                  y = "Density relative to the center") +
    plot_theme

  patchwork::wrap_plots(board, curve, nrow = 1, widths = c(1, 1.08))
}

bell_curve_rings_plot <- function() {
  ink <- "#24352F"
  green <- "#18453B"
  accent <- "#B85C25"
  muted <- "#74827B"
  theta <- seq(0, 2 * pi, length.out = 241)
  delta_s <- 0.5
  starts <- seq(0, 4.5, by = delta_s)

  # Every band has area pi * (r_outer^2 - r_inner^2) = pi * delta_s.
  rings <- do.call(rbind, lapply(seq_along(starts), function(i) {
    s <- starts[i]
    r_inner <- sqrt(s)
    r_outer <- sqrt(s + delta_s)
    data.frame(
      band = i, s = s, density = exp(-(s + delta_s/2)),
      x = c(r_outer * cos(theta), r_inner * cos(rev(theta))),
      y = c(r_outer * sin(theta), r_inner * sin(rev(theta)))
    )
  }))
  boundaries <- do.call(rbind, lapply(sqrt(starts + delta_s), function(radius) {
    data.frame(radius = radius, x = radius * cos(theta),
               y = radius * sin(theta))
  }))

  plot_theme <- ggplot2::theme_minimal(base_size = 16, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = ink, size = 17),
      plot.subtitle = ggplot2::element_text(color = muted, size = 14),
      axis.title = ggplot2::element_text(color = ink, size = 14),
      axis.text = ggplot2::element_text(color = ink, size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 16, 12, 12),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  board <- ggplot2::ggplot(rings, ggplot2::aes(x, y, group = band)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = density), color = NA) +
    ggplot2::scale_fill_gradient(low = "#F5F8F6", high = green,
                                 limits = c(0, 1), guide = "none") +
    ggplot2::geom_path(data = boundaries,
                      ggplot2::aes(x, y, group = radius), inherit.aes = FALSE,
                      color = "white", linewidth = 0.6) +
    ggplot2::geom_polygon(data = rings[rings$s == 1, ], fill = accent,
                         alpha = 0.75, color = NA) +
    ggplot2::geom_path(data = boundaries[boundaries$radius %in% sqrt(c(1, 1.5)), ],
                      ggplot2::aes(x, y, group = radius), inherit.aes = FALSE,
                      color = accent, linewidth = 0.7) +
    ggplot2::annotate("segment", x = 1.15, y = 1.3, xend = 0.8, yend = 0.8,
                      color = accent, linewidth = 0.7,
                      arrow = grid::arrow(length = grid::unit(0.09, "inches"))) +
    ggplot2::annotate("label", x = 1.08, y = 1.6, label = "s: 1 to 1.5",
                      color = accent, fill = "white", size = 4.3, linewidth = 0) +
    ggplot2::coord_fixed(xlim = c(-2.35, 2.35), ylim = c(-2.35, 2.35)) +
    ggplot2::labs(title = "Equal areas, thinner rings",
                  subtitle = expression(Delta*s == 0.5 ~ "for every ring")) +
    plot_theme + ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  rectangles <- data.frame(s = starts, end = starts + delta_s,
                            height = exp(-starts))
  curve <- data.frame(s = seq(0, 5, length.out = 501))
  curve$height <- exp(-curve$s)

  exponential <- ggplot2::ggplot(curve, ggplot2::aes(s, height)) +
    ggplot2::geom_rect(data = rectangles[rectangles$s != 1, ],
                      ggplot2::aes(xmin = s, xmax = end, ymin = 0, ymax = height),
                      inherit.aes = FALSE, fill = "#C8DCCF", color = "white",
                      linewidth = 0.7) +
    ggplot2::geom_rect(data = rectangles[rectangles$s == 1, ],
                      ggplot2::aes(xmin = s, xmax = end, ymin = 0, ymax = height),
                      inherit.aes = FALSE, fill = accent, alpha = 0.65,
                      color = accent, linewidth = 0.7) +
    ggplot2::geom_line(color = green, linewidth = 1.25) +
    ggplot2::annotate("text", x = 1.25, y = 0.17, label = "Delta*s",
                      parse = TRUE, color = "white", size = 4.8) +
    ggplot2::annotate("segment", x = 1.08, y = 0.055, xend = 1.42, yend = 0.055,
                      color = "white", linewidth = 0.7,
                      arrow = grid::arrow(ends = "both",
                                          length = grid::unit(0.07, "inches"))) +
    ggplot2::scale_x_continuous(breaks = 0:5, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, 1.05)) +
    ggplot2::labs(title = "Add rectangles instead",
                  subtitle = "The orange rectangle matches the orange ring",
                  x = expression("Squared radius, " * s == r^2),
                  y = expression("Density factor, " * e^{-s})) +
    plot_theme

  patchwork::wrap_plots(board, exponential, nrow = 1, widths = c(1, 1.08))
}
