# data_processing.R
# Data ingestion, source detection, standardization, and pitch classification

library(dplyr)
library(readr)

# ---- Strike Zone Constants (inches) ----
ZONE_LEFT    <- -8.5
ZONE_RIGHT   <-  8.5
ZONE_TOP_DEFAULT <- 42
ZONE_BOT_DEFAULT <- 18
BALL_RADIUS  <- 1.5    # zone expansion for baseball width

# ---- Source Detection ----
# Identifies the tracking system by looking for signature column names
detect_source <- function(col_names) {
  cols_lower <- tolower(col_names)

  trackman_sigs   <- c("taggedpitchtype", "autopitchtype", "spinaxis", "relspeed")
  flightscope_sigs <- c("horzbreak", "inducedvertbreak", "totalbreakz",
                          "break_horizontal", "break_induced_vertical",
                          "pitch_strike_zone_front")

  tm_hits <- sum(trackman_sigs %in% cols_lower)
  fs_hits <- sum(flightscope_sigs %in% cols_lower)

  if (tm_hits > fs_hits) return("Trackman")
  if (fs_hits > tm_hits) return("FlightScope")
  return("Unknown")
}

# ---- Column Standardisation ----
# Maps vendor-specific column names to a consistent internal schema:
#   plate_x, plate_z, pitch_call, zone_top, zone_bot
standardize_columns <- function(df, source) {
  cols       <- colnames(df)
  cols_lower <- tolower(cols)

  candidates <- list(
    plate_x    = c("platelocside",  "plate_loc_side",  "px"),
    plate_z    = c("platelocheight","plate_loc_height", "pz"),
    pitch_call = c("pitchcall",     "pitch_call",       "pitchresult"),
    zone_top   = c("strikezonetop", "strike_zone_top",  "sz_top"),
    zone_bot   = c("strikezonebottom","strike_zone_bottom","sz_bot")
  )

  find_col <- function(cands) {
    idx <- which(cols_lower %in% cands)
    if (length(idx) > 0) cols[idx[1]] else NA_character_
  }

  mapping <- lapply(candidates, find_col)

  for (std_name in names(mapping)) {
    orig <- mapping[[std_name]]
    if (!is.na(orig) && orig %in% colnames(df)) {
      df <- rename(df, !!std_name := !!rlang::sym(orig))
    }
  }

  # Ensure numeric types
  for (col in c("plate_x", "plate_z", "zone_top", "zone_bot")) {
    if (col %in% colnames(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  df
}

# ---- Pitch Call Standardisation ----
# Normalises vendor-specific call labels to "Ball" and "Called Strike"
standardize_pitch_calls <- function(df, source) {
  if (source == "Trackman") {
    df <- df %>%
      mutate(
        call_type = case_when(
          pitch_call %in% c("BallCalled", "BallinDirt", "BallIntentional") ~ "Ball",
          pitch_call == "StrikeCalled" ~ "Called Strike",
          TRUE ~ NA_character_
        )
      )
  } else {
    df <- df %>%
      mutate(
        call_type = case_when(
          tolower(pitch_call) %in% c("ball", "ballcalled", "ball called",
                                      "ball in dirt", "ballindirt",
                                      "b", "hbp") ~ "Ball",
          tolower(pitch_call) %in% c("called strike", "strikecalled",
                                      "strike called",
                                      "cs", "cso") ~ "Called Strike",
          TRUE ~ NA_character_
        )
      )
  }
  df
}

# ---- Filter to Taken Pitches ----
filter_taken_pitches <- function(df) {
  df %>%
    filter(!is.na(call_type), !is.na(plate_x), !is.na(plate_z))
}

# ---- Classify Pitches ----
# Places every taken pitch into one of four accuracy buckets:
#   Correct Strike, Correct Ball, False Positive (bad strike), False Negative (bad ball)
classify_pitches <- function(df) {
  df %>%
    mutate(
      z_top = if ("zone_top" %in% names(.)) coalesce(zone_top, ZONE_TOP_DEFAULT) else ZONE_TOP_DEFAULT,
      z_bot = if ("zone_bot" %in% names(.)) coalesce(zone_bot, ZONE_BOT_DEFAULT) else ZONE_BOT_DEFAULT,
      in_zone = plate_x >= (ZONE_LEFT - BALL_RADIUS) & plate_x <= (ZONE_RIGHT + BALL_RADIUS) &
                plate_z >= (z_bot - BALL_RADIUS)   & plate_z <= (z_top + BALL_RADIUS),
      classification = case_when(
        in_zone  & call_type == "Called Strike" ~ "Correct Strike",
        !in_zone & call_type == "Ball"          ~ "Correct Ball",
        !in_zone & call_type == "Called Strike"  ~ "False Positive",
        in_zone  & call_type == "Ball"           ~ "False Negative"
      )
    )
}

# ---- Summary Statistics ----
compute_summary <- function(df) {
  total     <- nrow(df)
  correct   <- sum(df$classification %in% c("Correct Strike", "Correct Ball"))
  false_pos <- sum(df$classification == "False Positive")
  false_neg <- sum(df$classification == "False Negative")

  accuracy_pct <- round(correct / total * 100, 1)

  # Consistency: lower spread of miss-distances from zone edge = more consistent

  misses <- df %>% filter(classification %in% c("False Positive", "False Negative"))
  if (nrow(misses) > 1) {
    miss_dist_x <- pmin(abs(misses$plate_x - ZONE_LEFT), abs(misses$plate_x - ZONE_RIGHT))
    miss_dist_z <- pmin(abs(misses$plate_z - misses$z_top), abs(misses$plate_z - misses$z_bot))
    spread  <- sd(miss_dist_x) + sd(miss_dist_z)
    consistency_score <- round(max(0, 100 - spread * 3.3), 1)
  } else {
    consistency_score <- 100
  }

  # Zone tendencies
  mid_z <- mean(c(ZONE_TOP_DEFAULT, ZONE_BOT_DEFAULT))
  zone_tendencies <- list(
    expanded_inside  = sum(false_pos & df$classification == "False Positive" &
                            abs(df$plate_x) <= 14.4, na.rm = TRUE),
    expanded_outside = sum(df$classification == "False Positive" &
                            abs(df$plate_x) > 14.4, na.rm = TRUE),
    missed_high      = sum(df$classification == "False Negative" &
                            df$plate_z > mid_z, na.rm = TRUE),
    missed_low       = sum(df$classification == "False Negative" &
                            df$plate_z <= mid_z, na.rm = TRUE)
  )

  data.frame(
    Metric = c("Total Taken Pitches",
               "Correct Calls",
               "Accuracy %",
               "False Positives (Bad Strikes)",
               "False Negatives (Bad Balls)",
               "Consistency Score",
               "Zone Tendency"),
    Value = c(
      total,
      correct,
      paste0(accuracy_pct, "%"),
      false_pos,
      false_neg,
      paste0(consistency_score, " / 100"),
      paste0("FP inside/outside: ", zone_tendencies$expanded_inside, "/",
             zone_tendencies$expanded_outside,
             "  |  FN high/low: ", zone_tendencies$missed_high, "/",
             zone_tendencies$missed_low)
    ),
    stringsAsFactors = FALSE
  )
}

# ---- Full Pipeline ----
# Convenience wrapper that runs the entire ingestion-to-classification pipeline
process_csv <- function(file_path) {
  raw <- read_csv(file_path, show_col_types = FALSE)
  source <- detect_source(colnames(raw))

  df <- raw %>%
    standardize_columns(source) %>%
    standardize_pitch_calls(source) %>%
    filter_taken_pitches() %>%
    classify_pitches()

  list(data = df, source = source, n_raw = nrow(raw))
}
