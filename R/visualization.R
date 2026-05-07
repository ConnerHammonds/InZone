# visualization.R
# ggplot2 plotting functions for umpire scorecard visualizations

library(ggplot2)

# ---- Colour Palette ----
CLASS_COLOURS <- c(
  "Correct Strike" = "#2ecc71",
  "Correct Ball"   = "#3498db",
  "False Positive"  = "#e74c3c",
  "False Negative"  = "#e67e22"
)

# ---- Base Theme + Strike Zone Layer ----
# Returns a list of ggplot layers that draw:
#   - the zone rectangle
#   - nine-zone grid lines
#   - home plate polygon
zone_layers <- function(z_top = 42, z_bot = 18, base = 13) {
  # Expanded zone boundaries (accounts for baseball width)
  ez_left   <- ZONE_LEFT - BALL_RADIUS
  ez_right  <- ZONE_RIGHT + BALL_RADIUS
  ez_bot    <- z_bot - BALL_RADIUS
  ez_top    <- z_top + BALL_RADIUS
  third_x <- (ez_right - ez_left) / 3
  third_z <- (ez_top - ez_bot) / 3

  list(
    # Strike zone outline (expanded)
    geom_rect(
      aes(xmin = ez_left, xmax = ez_right, ymin = ez_bot, ymax = ez_top),
      fill = NA, colour = "white", linewidth = 0.9, inherit.aes = FALSE
    ),
    # Vertical grid lines (inner zone thirds)
    geom_segment(aes(x = ez_left + third_x, xend = ez_left + third_x,
                     y = ez_bot, yend = ez_top),
                 colour = "grey50", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = ez_left + 2 * third_x, xend = ez_left + 2 * third_x,
                     y = ez_bot, yend = ez_top),
                 colour = "grey50", linetype = "dashed", inherit.aes = FALSE),
    # Horizontal grid lines (inner zone thirds)
    geom_segment(aes(x = ez_left, xend = ez_right,
                     y = ez_bot + third_z, yend = ez_bot + third_z),
                 colour = "grey50", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = ez_left, xend = ez_right,
                     y = ez_bot + 2 * third_z, yend = ez_bot + 2 * third_z),
                 colour = "grey50", linetype = "dashed", inherit.aes = FALSE),
    # Home plate
    geom_polygon(
      data = data.frame(
        x = c(-8.5, 8.5, 8.5, 0, -8.5),
        y = c(1.2, 1.2, 3.0, 5.4, 3.0)
      ),
      aes(x = x, y = y), fill = "#333333", colour = "white",
      linewidth = 0.5, inherit.aes = FALSE
    ),
    # Catcher's view labels
    labs(x = "Horizontal Location (in)", y = "Vertical Location (in)"),
    coord_fixed(xlim = c(-24, 24), ylim = c(0, 60)),
    theme_minimal(base_size = base),
    theme(
      panel.background = element_rect(fill = "#222222", color = NA),
      plot.background = element_rect(fill = "#222222", color = NA),
      plot.margin = margin(2, 2, 2, 2),
      text = element_text(color = "white"),
      axis.text = element_text(color = "#cccccc"),
      panel.grid.major = element_line(color = "#444444"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "white"),
      plot.subtitle = element_text(color = "#cccccc", hjust = 0.5),
      legend.background = element_rect(fill = "#222222"),
      legend.text = element_text(color = "#cccccc"),
      legend.title = element_text(color = "white")
    )
  )
}

# ---- Plot 1: False Positives (Balls called Strikes) ----
plot_false_positives <- function(df, point_size = 3.5, base = 13) {
  fp <- df %>% filter(classification == "False Positive")

  ggplot(fp, aes(x = -plate_x, y = plate_z)) +
    zone_layers(base = base) +
    geom_point(colour = "#e74c3c", size = point_size, alpha = 0.8) +
    labs(
      title = "False Positives",
      subtitle = paste(nrow(fp), "bad strike calls")
    )
}

# ---- Plot 2: False Negatives (Strikes called Balls) ----
plot_false_negatives <- function(df, point_size = 3.5, base = 13) {
  fn <- df %>% filter(classification == "False Negative")

  ggplot(fn, aes(x = -plate_x, y = plate_z)) +
    zone_layers(base = base) +
    geom_point(colour = "#e67e22", size = point_size, alpha = 0.8) +
    labs(
      title = "False Negatives",
      subtitle = paste(nrow(fn), "bad ball calls")
    )
}

# ---- Plot 3: Overall Map ----
plot_overall <- function(df, point_size = 3, base = 13) {
  ggplot(df, aes(x = -plate_x, y = plate_z, colour = classification)) +
    zone_layers(base = base) +
    geom_point(size = point_size, alpha = 0.75) +
    scale_colour_manual(values = CLASS_COLOURS, name = "Call Result") +
    labs(
      title = "Overall Pitch Map",
      subtitle = paste(nrow(df), "taken pitches")
    )
}
