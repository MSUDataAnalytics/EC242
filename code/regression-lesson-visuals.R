# Figures for the October 15 regression lesson. The lesson supplies the fixed,
# complete-case county dataset; no network requests occur here.
regression_lesson_figures <- function(counties) {
  library(ggplot2)
  ink <- "#24352F"
  green <- "#18453B"
  orange <- "#B85C25"
  blue <- "#326A9B"
  gray <- "#8B9691"
  fit <- lm(IncomeK ~ CollegePct, data = counties)
  b <- coef(fit)
  income_mean <- mean(counties$IncomeK)
  county_theme <- theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", color = ink, size = 17),
      plot.subtitle = element_text(color = "#67746E", size = 13),
      plot.caption = element_text(color = "#67746E"),
      axis.title = element_text(color = ink, size = 13),
      axis.text = element_text(color = ink, size = 11),
      strip.text = element_text(face = "bold", color = ink, size = 13),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 14, 12, 12)
    )
  dollar_k <- scales::label_dollar(suffix = "k", accuracy = 1)
  scatter <- function() {
    ggplot(counties, aes(CollegePct, IncomeK)) +
      geom_point(color = gray, alpha = 0.20, size = 0.75) +
      scale_y_continuous(labels = dollar_k) +
      labs(x = "College-educated adults (%)",
           y = "Median household income") +
      county_theme
  }
  focus <- counties[counties$State == "Michigan" &
                      counties$County %in% c("Ingham County", "Ottawa County"), ]
  focus$label_x <- c(52, 48)[match(focus$County, c("Ingham County", "Ottawa County"))]
  focus$label_y <- c(48, 108)[match(focus$County, c("Ingham County", "Ottawa County"))]
  opening <- ggplot(counties[order(counties$State == "Michigan"), ],
                    aes(CollegePct, IncomeK,
                        color = ifelse(State == "Michigan", "Michigan", "Other counties"))) +
    geom_point(alpha = 0.32, size = 1) +
    geom_segment(data = focus,
                 aes(xend = label_x, yend = label_y),
                 color = orange, linewidth = 0.55) +
    geom_label(data = focus,
                aes(x = label_x, y = label_y, label = sub(" County", "", County)),
                color = orange, fill = "white", linewidth = 0, size = 4) +
    scale_color_manual(values = c("Michigan" = orange, "Other counties" = gray)) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    scale_y_continuous(labels = dollar_k) +
    labs(title = "What does education tell us about a county's income?",
          subtitle = "Each dot is one county or county equivalent",
          x = "Adults 25+ with a bachelor's degree or more (%)",
          y = "Median household income") +
    county_theme

  baseline <- patchwork::wrap_plots(
    scatter() +
      geom_hline(yintercept = income_mean, color = orange, linewidth = 1) +
      labs(title = "One guess for every county",
            subtitle = sprintf("Average county median: %s", dollar_k(income_mean))),
    scatter() +
      geom_abline(intercept = b[1], slope = b[2], color = green, linewidth = 1) +
      labs(title = "Use what we know about X",
            subtitle = "Predictions change with education"),
    nrow = 1
  )

  walk <- data.frame(x = c(30, 40), y = as.numeric(predict(fit,
                                          newdata = data.frame(CollegePct = c(30, 40)))))
  slope <- scatter() +
    geom_abline(intercept = b[1], slope = b[2], color = green, linewidth = 1) +
    geom_segment(aes(x = 30, xend = 40, y = walk$y[1], yend = walk$y[1]),
                  inherit.aes = FALSE, data = data.frame(one = 1),
                  color = orange, linewidth = 1.3) +
    geom_segment(aes(x = 40, xend = 40, y = walk$y[1], yend = walk$y[2]),
                  inherit.aes = FALSE, data = data.frame(one = 1),
                  color = orange, linewidth = 1.3) +
    geom_point(data = walk, aes(x, y), inherit.aes = FALSE,
                color = orange, size = 3) +
    annotate("label", x = 35, y = walk$y[1] - 9, label = "+10 percentage points",
              color = orange, fill = "white", linewidth = 0, size = 4) +
    annotate("label", x = 54, y = mean(walk$y), label = sprintf("+%s", dollar_k(diff(walk$y))),
              color = orange, fill = "white", linewidth = 0, size = 4) +
    labs(title = "A slope is a change in Y for a change in X",
          subtitle = sprintf("Ten percentage points correspond to about %s in fitted income",
                              dollar_k(10 * b[2])))

  units <- patchwork::wrap_plots(
    ggplot(counties, aes(CollegePct, IncomeK)) +
      geom_point(alpha = 0.17, color = gray, size = 0.7) +
      geom_abline(intercept = b[1], slope = b[2], color = green, linewidth = 1) +
      labs(title = "Income in thousands of dollars",
            subtitle = sprintf("Slope = %.3f", b[2]), x = "College-educated adults (%)",
            y = "Income ($1,000s)") + county_theme,
    ggplot(counties, aes(CollegePct, MedianIncome)) +
      geom_point(alpha = 0.17, color = gray, size = 0.7) +
      geom_abline(intercept = 1000 * b[1], slope = 1000 * b[2],
                    color = green, linewidth = 1) +
      scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
      labs(title = "Income in dollars",
            subtitle = sprintf("Slope = %s", scales::comma(1000 * b[2], accuracy = 0.1)),
            x = "College-educated adults (%)", y = "Income ($)") + county_theme,
    nrow = 1
  )

  selected <- counties[counties$State == "Michigan" &
                          counties$County %in% c("Ingham County", "Ottawa County",
                                                 "Washtenaw County"), ]
  selected$fitted <- as.numeric(predict(fit, selected))
  selected$residual <- selected$IncomeK - selected$fitted
  selected$label_x <- c(49, 28, 68)[match(selected$County,
                                          c("Ingham County", "Ottawa County", "Washtenaw County"))]
  selected$label_y <- c(52, 110, 77)[match(selected$County,
                                           c("Ingham County", "Ottawa County", "Washtenaw County"))]
  selected$label <- paste0(sub(" County", "", selected$County), "\n",
                            sprintf("%+.1fk", selected$residual))
  residuals <- scatter() +
    geom_abline(intercept = b[1], slope = b[2], color = green, linewidth = 1) +
    geom_segment(data = selected, aes(xend = CollegePct, yend = fitted),
                   color = orange, linewidth = 1.15) +
    geom_point(data = selected, color = orange, size = 3) +
    geom_point(data = selected, aes(y = fitted), color = orange,
                 fill = "white", shape = 21, size = 3, stroke = 1) +
    geom_segment(data = selected, aes(xend = label_x, yend = label_y),
                   color = orange, linewidth = 0.4, linetype = "dotted") +
    geom_label(data = selected, aes(x = label_x, y = label_y, label = label),
                 fill = "white", color = orange, linewidth = 0, size = 3.7) +
    labs(title = "The vertical gap is the residual",
          subtitle = "Filled point: observed income. Open point: fitted income.")

  slopes <- b[2] * c(0.5, 1, 1.5)
  candidates <- do.call(rbind, lapply(seq_along(slopes), function(i) {
    slope_i <- slopes[i]
    intercept_i <- income_mean - slope_i * mean(counties$CollegePct)
    error <- counties$IncomeK - intercept_i - slope_i * counties$CollegePct
    data.frame(counties, intercept = intercept_i, slope = slope_i,
                 candidate = sprintf("%s\nRMSE = %s",
                    c("Too flat", "OLS fit", "Too steep")[i], dollar_k(sqrt(mean(error^2)))))
  }))
  candidates$candidate <- factor(candidates$candidate,
                                  levels = unique(candidates$candidate))
  candidate_lines <- unique(candidates[c("candidate", "intercept", "slope")])
  squared_loss <- ggplot(candidates, aes(CollegePct, IncomeK)) +
    geom_point(alpha = 0.12, color = gray, size = 0.5) +
    geom_abline(data = candidate_lines, aes(intercept = intercept, slope = slope),
                   color = green, linewidth = 1) +
    facet_wrap(~candidate, nrow = 1) +
    scale_y_continuous(labels = dollar_k) +
    labs(title = "Choose a line by the errors it makes",
          subtitle = "Each candidate passes through the sample means; OLS minimizes squared vertical gaps",
          x = "College-educated adults (%)", y = "Median household income") +
    county_theme

  diagnostic_data <- data.frame(fitted = fitted(fit), residual = resid(fit))
  diagnostics <- patchwork::wrap_plots(
    ggplot(diagnostic_data, aes(fitted, residual)) +
      geom_point(alpha = 0.2, color = gray, size = 0.8) +
      geom_hline(yintercept = 0, color = green, linewidth = 0.8) +
      scale_x_continuous(labels = dollar_k) +
      scale_y_continuous(labels = dollar_k) +
      labs(title = "Do errors have a pattern?", x = "Fitted income",
            y = "Observed minus fitted income") + county_theme,
    ggplot(diagnostic_data, aes(residual)) +
      geom_histogram(binwidth = 5, boundary = 0, fill = green, color = "white") +
      geom_vline(xintercept = 0, color = orange, linewidth = 0.8) +
      scale_x_continuous(labels = dollar_k) +
      labs(title = "How large are the misses?", x = "Observed minus fitted income",
            y = "Number of counties") + county_theme,
    nrow = 1
  )

  predictors <- c("CollegePct", "PovertyPct", "MedianAge")
  labels <- c("College-educated adults (%)", "Residents below poverty (%)", "Median age (years)")
  comparison <- do.call(rbind, lapply(seq_along(predictors), function(i) {
    model <- lm(reformulate(predictors[i], "IncomeK"), data = counties)
    data.frame(x = counties[[predictors[i]]], IncomeK = counties$IncomeK,
                 panel = sprintf("%s\nSlope = %.2f; R² = %.2f", labels[i],
                                   coef(model)[2], summary(model)$r.squared))
  }))
  comparison$panel <- factor(comparison$panel, levels = unique(comparison$panel))
  compare <- ggplot(comparison, aes(x, IncomeK)) +
    geom_point(color = gray, alpha = 0.15, size = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = green, linewidth = 1) +
    facet_wrap(~panel, scales = "free_x", nrow = 1) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    scale_y_continuous(labels = dollar_k) +
    labs(title = "Positive, negative, and barely useful",
          subtitle = "Three separate regressions, each with one predictor and the same counties",
          x = NULL, y = "Median household income") +
    county_theme + theme(panel.spacing.x = grid::unit(1.1, "lines"))

  numeric_data <- counties[c("CollegePct", "IncomeK", "PovertyPct", "MedianAge")]
  correlation_matrix <- cor(numeric_data)
  dimnames(correlation_matrix) <- list(c("College", "Income", "Poverty", "Age"),
                                        c("College", "Income", "Poverty", "Age"))
  correlations <- as.data.frame(as.table(correlation_matrix))
  correlations <- correlations[as.integer(correlations$Var1) < as.integer(correlations$Var2), ]
  heatmap <- ggplot(correlations, aes(Var1, Var2, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", Freq)), color = ink, size = 5) +
    scale_fill_gradient2(low = "#D9A27F", mid = "white", high = "#8DB4A3",
                           limits = c(-1, 1), breaks = c(-1, 0, 1),
                           name = "Correlation",
                           guide = guide_colorbar(barwidth = grid::unit(6, "cm"),
                                                  barheight = grid::unit(0.4, "cm"))) +
    coord_equal() +
    labs(title = "Correlation keeps the sign",
          subtitle = "Unit-free; the same in either direction",
          x = NULL, y = NULL) +
    county_theme + theme(panel.grid = element_blank())

  set.seed(242)
  x <- seq(-2, 2, length.out = 160)
  simulated <- rbind(
    data.frame(x, y = 1.4 * x + rnorm(length(x), sd = 0.6), shape = "Positive"),
    data.frame(x, y = -1.4 * x + rnorm(length(x), sd = 0.6), shape = "Negative"),
    data.frame(x, y = x^2 + rnorm(length(x), sd = 0.18), shape = "Curved")
  )
  for (shape_i in unique(simulated$shape)) {
    at <- simulated$shape == shape_i
    simulated$panel[at] <- sprintf("%s\nr = %.2f", shape_i,
                                     cor(simulated$x[at], simulated$y[at]))
  }
  simulated$panel <- factor(simulated$panel, levels = unique(simulated$panel))
  curves <- ggplot(simulated, aes(x, y)) +
    geom_point(color = green, alpha = 0.5, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, color = orange, linewidth = 0.9) +
    facet_wrap(~panel, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = c(-2, 0, 2)) +
    labs(title = "A small correlation can hide a strong pattern",
          subtitle = "Simulated examples; orange lines are ordinary least-squares fits",
          x = "X", y = "Y") +
    county_theme + theme(panel.spacing.x = grid::unit(1.1, "lines"))

  base_points <- data.frame(x = seq(1, 6, length.out = 25))
  base_points$y <- 8 + 3 * base_points$x + rnorm(nrow(base_points), sd = 1.3)
  unusual <- data.frame(x = c(mean(base_points$x), 11), y = c(42, 2),
                         panel = c("Large residual, ordinary X", "Unusual X and a large residual"))
  influence_points <- rbind(
    data.frame(base_points, panel = unusual$panel[1]),
    data.frame(base_points, panel = unusual$panel[2])
  )
  original <- lm(y ~ x, data = base_points)
  lines <- do.call(rbind, lapply(seq_len(nrow(unusual)), function(i) {
    changed <- lm(y ~ x, data = rbind(base_points, unusual[i, c("x", "y")]))
    data.frame(panel = unusual$panel[i], which = c("Original fit", "With highlighted point"),
                 intercept = c(coef(original)[1], coef(changed)[1]),
                 slope = c(coef(original)[2], coef(changed)[2]))
  }))
  influence <- ggplot(influence_points, aes(x, y)) +
    geom_point(color = gray, size = 1.6) +
    geom_abline(data = lines, aes(intercept = intercept, slope = slope, color = which),
                   linewidth = 1) +
    geom_point(data = unusual, color = orange, size = 3.7) +
    facet_wrap(~panel, nrow = 1) +
    scale_color_manual(values = c("Original fit" = green, "With highlighted point" = orange)) +
    coord_cartesian(xlim = c(0, 12), ylim = c(0, 48)) +
    labs(title = "A surprising outcome and an influential point are different things",
          subtitle = "Simulated examples; compare how much the fitted slope changes",
          x = "X", y = "Y") +
    county_theme

  observed <- range(counties$CollegePct)
  line_x <- data.frame(CollegePct = seq(0, 100, by = 0.1))
  line_x$IncomeK <- as.numeric(predict(fit, line_x))
  line_x$where <- ifelse(line_x$CollegePct >= observed[1] &
                            line_x$CollegePct <= observed[2], "Observed range", "Extrapolation")
  extrapolation <- ggplot(counties, aes(CollegePct, IncomeK)) +
    annotate("rect", xmin = observed[1], xmax = observed[2], ymin = -Inf, ymax = Inf,
               fill = "#E9F0EC", alpha = 0.65) +
    geom_point(alpha = 0.15, color = gray, size = 0.65) +
    geom_line(data = line_x, aes(color = where, linetype = where), linewidth = 1) +
    geom_vline(xintercept = observed, color = gray, linetype = "dotted") +
    scale_color_manual(values = c("Observed range" = green, "Extrapolation" = orange)) +
    scale_linetype_manual(values = c("Observed range" = "solid", "Extrapolation" = "dashed")) +
    scale_y_continuous(labels = dollar_k) +
    labs(title = "R will happily predict where we have no observations",
          subtitle = "The shaded band marks the observed education range",
          x = "College-educated adults (%)", y = "Median household income") +
    county_theme

  sample_lines <- do.call(rbind, lapply(seq_len(8), function(i) {
    sample_i <- counties[sample.int(nrow(counties), 50), ]
    model <- lm(IncomeK ~ CollegePct, data = sample_i)
    data.frame(intercept = coef(model)[1], slope = coef(model)[2])
  }))
  sampling <- scatter() +
    geom_abline(data = sample_lines, aes(intercept = intercept, slope = slope),
                   color = orange, alpha = 0.6, linewidth = 0.75) +
    geom_abline(intercept = b[1], slope = b[2], color = green, linewidth = 1.4) +
    labs(title = "New sample, new coefficients",
          subtitle = "Orange: eight samples of 50 counties. Green: the full county file.")

  list(opening = opening, baseline = baseline, slope = slope, units = units,
         residuals = residuals, squared_loss = squared_loss, diagnostics = diagnostics,
         compare = compare, heatmap = heatmap, curves = curves,
         influence = influence, extrapolation = extrapolation, sampling = sampling)
}
