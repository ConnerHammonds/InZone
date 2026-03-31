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
zone_layers <- function(z_top = 3.5, z_bot = 1.5) {
  third_x <- (ZONE_RIGHT - ZONE_LEFT) / 3
  third_z <- (z_top - z_bot) / 3

  list(
    # Strike zone outline
    geom_rect(
      aes(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = z_bot, ymax = z_top),
      fill = NA, colour = "black", linewidth = 0.9, inherit.aes = FALSE
    ),
    # Vertical grid lines (inner zone thirds)
    geom_segment(aes(x = ZONE_LEFT + third_x, xend = ZONE_LEFT + third_x,
                     y = z_bot, yend = z_top),
                 colour = "grey60", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = ZONE_LEFT + 2 * third_x, xend = ZONE_LEFT + 2 * third_x,
                     y = z_bot, yend = z_top),
                 colour = "grey60", linetype = "dashed", inherit.aes = FALSE),
    # Horizontal grid lines (inner zone thirds)
    geom_segment(aes(x = ZONE_LEFT, xend = ZONE_RIGHT,
                     y = z_bot + third_z, yend = z_bot + third_z),
                 colour = "grey60", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = ZONE_LEFT, xend = ZONE_RIGHT,
                     y = z_bot + 2 * third_z, yend = z_bot + 2 * third_z),
                 colour = "grey60", linetype = "dashed", inherit.aes = FALSE),
    # Home plate
    geom_polygon(
      data = data.frame(
        x = c(-0.708, 0.708, 0.708, 0, -0.708),
        y = c(0.1, 0.1, 0.25, 0.45, 0.25)
      ),
      aes(x = x, y = y), fill = "white", colour = "black",
      linewidth = 0.5, inherit.aes = FALSE
    ),
    # Catcher's view labels
    labs(x = "Horizontal Location (ft)", y = "Vertical Location (ft)"),
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)),
    theme_minimal(base_size = 13),
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  )
}

# ---- Plot 1: False Positives (Balls called Strikes) ----
plot_false_positives <- function(df) {
  fp <- df %>% filter(classification == "False Positive")

  ggplot(fp, aes(x = plate_x, y = plate_z)) +
    zone_layers() +
    geom_point(colour = "#e74c3c", size = 3.5, alpha = 0.8) +
    labs(
      title = "False Positives — Balls Called Strikes",
      subtitle = paste(nrow(fp), "incorrect strike calls")
    )
}

# ---- Plot 2: False Negatives (Strikes called Balls) ----
plot_false_negatives <- function(df) {
  fn <- df %>% filter(classification == "False Negative")

  ggplot(fn, aes(x = plate_x, y = plate_z)) +
    zone_layers() +
    geom_point(colour = "#e67e22", size = 3.5, alpha = 0.8) +
    labs(
      title = "False Negatives — Strikes Called Balls",
      subtitle = paste(nrow(fn), "incorrect ball calls")
    )
}

# ---- Plot 3: Overall Map ----
plot_overall <- function(df) {
  ggplot(df, aes(x = plate_x, y = plate_z, colour = classification)) +
    zone_layers() +
    geom_point(size = 3, alpha = 0.75) +
    scale_colour_manual(values = CLASS_COLOURS, name = "Call Result") +
    labs(
      title = "Overall Taken-Pitch Map",
      subtitle = paste(nrow(df), "total taken pitches")
    )
}
