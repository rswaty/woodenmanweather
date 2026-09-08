# Wooden Man Weather — Outpost Top Row Widgets (Option 1: Field Slate & Forest Pine)
# Provides:
# 1. Global Human Thermometer (Instrument Readout + Percentile Bar + Stats)
# 2. Big Lake & Woodstove Gauge (Lake Superior, Breeze, Heating Index, Daylight Arc)

source("R/constants.R")

#' Global Human Thermometer Model
#' Estimates where Marquette's current temperature ranks against the world's 8.16B human population.
wmw_calc_human_thermometer <- function(temp_f, date = Sys.Date()) {
  doy <- as.integer(format(date, "%j"))
  pop_total <- 8.16 # billion humans in 2026

  # Global population-weighted median oscillates between ~57.5°F (Jan) and ~74.5°F (late July)
  # Peaks around doy 205 (July 24)
  rad <- 2 * pi * (doy - 205) / 365
  global_median <- 66 + 8.5 * cos(rad)
  global_sd <- 13.5 - 2.5 * cos(rad)

  z <- (temp_f - global_median) / global_sd
  pct_warmer <- stats::pnorm(z) * 100
  pct_warmer <- max(0.5, min(99.5, pct_warmer))

  warmer_than_b <- pop_total * (pct_warmer / 100)
  colder_than_b <- pop_total - warmer_than_b

  is_warmer_than_half <- pct_warmer >= 50

  list(
    local_temp = round(temp_f, 0),
    percentile = round(pct_warmer, 0),
    warmer_than_billions = round(warmer_than_b, 1),
    colder_than_billions = round(colder_than_b, 1),
    global_median = round(global_median, 0),
    is_warmer_than_half = is_warmer_than_half
  )
}

#' Lake Superior Water Temperature Model (Marquette Harbor Climatology)
wmw_calc_lake_superior_temp <- function(date = Sys.Date()) {
  doy <- as.integer(format(date, "%j"))
  # Peaks around Sept 2 (doy 245) at ~62°F harbor, lowest in March at ~34°F
  rad <- 2 * pi * (doy - 245) / 365
  harbor_temp <- round(48 + 14 * cos(rad), 0)
  open_lake_temp <- round(44 + 11 * cos(rad), 0)
  list(harbor = harbor_temp, open = open_lake_temp)
}

#' Solar Daylight Arc for Marquette (46.5436° N)
wmw_calc_daylight_arc <- function(date = Sys.Date()) {
  lat <- 46.5436
  doy1 <- as.integer(format(date, "%j"))
  doy2 <- doy1 + 1

  calc_hours <- function(doy) {
    p <- 0.0172024 * (doy - 81)
    dec <- asin(sin(23.44 * pi / 180) * sin(p))
    lat_r <- lat * pi / 180
    h <- acos(-tan(lat_r) * tan(dec))
    (2 * h) / (15 * pi / 180)
  }

  h1 <- calc_hours(doy1)
  h2 <- calc_hours(doy2)

  total_hrs <- floor(h1)
  total_mins <- round((h1 - total_hrs) * 60)

  diff_sec <- round((h2 - h1) * 3600)
  diff_mins_abs <- abs(diff_sec) %/% 60
  diff_sec_rem <- abs(diff_sec) %% 60
  sign_str <- if (diff_sec >= 0) "+" else "-"

  list(
    length_str = paste0(total_hrs, "h ", total_mins, "m"),
    change_str = paste0(sign_str, diff_mins_abs, "m ", diff_sec_rem, "s/day"),
    is_decreasing = diff_sec < 0
  )
}

#' Flannel & Woodstove Rating
wmw_calc_woodstove_index <- function(today_high) {
  if (is.null(today_high) || is.na(today_high)) today_high <- 70

  if (today_high >= 78) {
    list(level = "Level 1", title = "Screen Doors & Porches", desc = "Windows open · No stove", color = "#fbbf24")
  } else if (today_high >= 65) {
    list(level = "Level 2", title = "Light Flannel", desc = "Windows cracked · Morning chill", color = "#34d399")
  } else if (today_high >= 50) {
    list(level = "Level 3", title = "Evening Stove Draft", desc = "Take the chill off the cabin", color = "#38bdf8")
  } else if (today_high >= 32) {
    list(level = "Level 4", title = "Daily Stoking", desc = "Steady hardwood burn · Wool socks", color = "#c084fc")
  } else if (today_high >= 15) {
    list(level = "Level 5", title = "Deep Firebox", desc = "24/7 draft · Keep kettle boiling", color = "#f87171")
  } else {
    list(level = "Level 6", title = "Polar Vortex", desc = "Full draft · Check the woodpile", color = "#fb7185")
  }
}

#' Lake Breeze & Surf State
wmw_calc_lake_breeze <- function(wind_dir, wind_speed) {
  if (is.null(wind_dir) || is.na(wind_dir) || nchar(wind_dir) == 0) {
    return(list(state = "Calm / Variable", color = "#34d399", desc = "Glassy shore"))
  }

  dir_upper <- toupper(wind_dir)
  if (grepl("E|NE|SE", dir_upper)) {
    list(state = "Onshore Lake Breeze", color = "#38bdf8", desc = "Choppy surf · Cooler in city")
  } else if (grepl("S|SW|W", dir_upper)) {
    list(state = "Offshore Breeze", color = "#34d399", desc = "Calm near shore · Beach warmth")
  } else {
    list(state = "Northerly Wind", color = "#cbd5e1", desc = "Lake rollers · Canadian air")
  }
}

#' CARD 1: Global Human Thermometer Widget
wmw_card_human_thermometer <- function(today_high) {
  if (is.null(today_high) || is.na(today_high)) today_high <- 73
  ht <- wmw_calc_human_thermometer(today_high)

  accent_color <- if (ht$is_warmer_than_half) "#f59e0b" else "#38bdf8"
  badge_text <- paste0("DAILY HIGH RANK: ", ht$percentile, "%")

  headline_html <- if (ht$is_warmer_than_half) {
    paste0(
      "Today's high in Marquette of <span class='wmw-hl-warm'>", ht$local_temp, "°F</span> puts you warmer than <span class='wmw-hl-warm'>",
      ht$percentile, "% of humanity</span>."
    )
  } else {
    colder_pct <- 100 - ht$percentile
    paste0(
      "Today's high in Marquette of <span class='wmw-hl-cold'>", ht$local_temp, "°F</span> leaves you colder than <span class='wmw-hl-cold'>",
      colder_pct, "% of humanity</span>."
    )
  }

  subtext_html <- if (ht$is_warmer_than_half) {
    paste0("Out of 8.2 billion people on Earth today, approximately <strong>", ht$warmer_than_billions, " billion</strong> have a cooler daytime high.")
  } else {
    paste0("Out of 8.2 billion people on Earth today, approximately <strong>", ht$colder_than_billions, " billion</strong> have a warmer daytime high.")
  }

  # Scale bar position (clamped 4% to 96% for pin visibility)
  pin_pct <- max(4, min(96, ht$percentile))

  html <- paste0(
"<div class='wmw-outpost-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Global Human Thermometer</span>
    <span class='wmw-op-badge' style='border-color: ", accent_color, "40; color: ", accent_color, ";'>", badge_text, "</span>
  </div>
  <div class='wmw-op-body'>
    <p class='wmw-human-headline'>", headline_html, "</p>
    <p class='wmw-human-sub'>", subtext_html, "</p>
    
    <div class='wmw-earth-bar-shell'>
      <div class='wmw-earth-bar-track'>
        <div class='wmw-earth-bar-pin' style='left: ", pin_pct, "%; border-color: ", accent_color, ";'></div>
      </div>
      <div class='wmw-earth-bar-labels'>
        <span>Colder (0%)</span>
        <span>Global Median (", ht$global_median, "°F)</span>
        <span>Warmer (100%)</span>
      </div>
    </div>
  </div>
  <div class='wmw-op-footer'>
    <div class='wmw-stat-pill'>Marquette High: <strong>", ht$local_temp, "°F</strong></div>
    <div class='wmw-stat-pill'>Global Median: <strong>", ht$global_median, "°F</strong></div>
    <div class='wmw-stat-pill'>Coldest: <strong>-62°F (Vostok)</strong></div>
    <div class='wmw-stat-pill'>Warmest: <strong>114°F (Kuwait)</strong></div>
  </div>
</div>")

  htmltools::HTML(html)
}

#' CARD 2: Big Lake & Woodstove Gauge Widget
wmw_card_lake_woodstove <- function(today_high, forecast_df, date = Sys.Date()) {
  lake <- wmw_calc_lake_superior_temp(date)
  daylight <- wmw_calc_daylight_arc(date)
  stove <- wmw_calc_woodstove_index(today_high)

  # Extract first period wind
  first_wind_dir <- if (nrow(forecast_df) > 0) forecast_df$wind_direction[1] else "S"
  first_wind_speed <- if (nrow(forecast_df) > 0) forecast_df$wind_speed[1] else "10 mph"
  surf <- wmw_calc_lake_breeze(first_wind_dir, first_wind_speed)

  daylight_color <- if (daylight$is_decreasing) "#fca5a5" else "#86efac"

  update_time <- lubridate::with_tz(Sys.time(), "America/Detroit")
  update_str <- trimws(gsub("  ", " ", format(update_time, "%a, %b %e · %l:%M %p %Z")))

  html <- paste0(
"<div class='wmw-outpost-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Big Lake & Woodstove</span>
    <span class='wmw-op-badge' style='border-color: rgba(56, 189, 248, 0.45); color: #38bdf8;'>Lake Superior</span>
  </div>
  <div class='wmw-op-body'>
    <div class='wmw-gauge-list'>
      <div class='wmw-gauge-item'>
        <span class='wmw-gauge-label'>Lake Superior Water</span>
        <span class='wmw-gauge-val'><strong style='color: #38bdf8;'>", lake$harbor, "°F</strong> <span class='wmw-gauge-sub'>Harbor</span> &nbsp;·&nbsp; <strong style='color: #7dd3fc;'>", lake$open, "°F</strong> <span class='wmw-gauge-sub'>Open Lake</span></span>
      </div>
      <div class='wmw-gauge-item'>
        <span class='wmw-gauge-label'>Lake Breeze / Surf</span>
        <span class='wmw-gauge-val'><strong style='color: ", surf$color, ";'>", surf$state, "</strong> <span class='wmw-gauge-sub'>— ", surf$desc, "</span></span>
      </div>
      <div class='wmw-gauge-item'>
        <span class='wmw-gauge-label'>Flannel & Stove Index</span>
        <span class='wmw-gauge-val'><strong style='color: ", stove$color, ";'>", stove$level, ": ", stove$title, "</strong> <span class='wmw-gauge-sub'>— ", stove$desc, "</span></span>
      </div>
      <div class='wmw-gauge-item'>
        <span class='wmw-gauge-label'>Daylight Arc</span>
        <span class='wmw-gauge-val'><strong style='color: #ffffff;'>", daylight$length_str, "</strong> <span class='wmw-gauge-sub' style='color: ", daylight_color, "; font-weight: 600;'>&nbsp;(", daylight$change_str, ")</span></span>
      </div>
    </div>
  </div>
  <div class='wmw-op-footer'>
    <div class='wmw-stat-pill'>Last Updated: <strong>", update_str, "</strong></div>
    <div class='wmw-stat-pill'>Station: <strong>Marquette 46.54°N</strong></div>
  </div>
</div>")

  htmltools::HTML(html)
}

#' CARD 3: Left Column Notes & Logo Placeholder Widget
wmw_card_sidebar_dispatch <- function() {
  html <- paste0(
"<div class='wmw-outpost-card wmw-sidebar-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Outpost Dispatch</span>
    <span class='wmw-op-badge' style='border-color: rgba(245, 158, 11, 0.4); color: #f59e0b;'>Station Notes</span>
  </div>
  
  <div class='wmw-op-body' style='justify-content: flex-start; gap: 12px;'>
    <!-- Logo Placeholder Box -->
    <div class='wmw-logo-box'>
      <div class='wmw-logo-emblem'>
        <svg width='30' height='30' viewBox='0 0 24 24' fill='none' stroke='#38bdf8' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'>
          <path d='M12 2L2 7l10 5 10-5-10-5z'></path>
          <path d='M2 17l10 5 10-5'></path>
          <path d='M2 12l10 5 10-5'></path>
        </svg>
      </div>
      <div class='wmw-logo-title'>Wooden Man Weather</div>
      <div class='wmw-logo-sub'>Marquette · Lake Superior Outpost</div>
      <div class='wmw-logo-tag'>[ Logo Placeholder ]</div>
    </div>

    <!-- Dispatch & Notes Section -->
    <div class='wmw-notes-container'>
      <div class='wmw-notes-header'>FIELD &amp; COMMUNITY NOTES</div>
      
      <div class='wmw-note-entry'>
        <div class='wmw-note-date'>Seasonal Advisory</div>
        <div class='wmw-note-text'>Early September lake breezes keep shore brisk. Check evening drafts as overnight lows begin dipping.</div>
      </div>

      <div class='wmw-note-entry'>
        <div class='wmw-note-date'>Woodpile Prep</div>
        <div class='wmw-note-text'>Ensure split hardwood is stacked and top-covered before late September lake gales set in.</div>
      </div>

      <div class='wmw-note-entry'>
        <div class='wmw-note-date'>Monday Briefing</div>
        <div class='wmw-note-text'>Work-week task timing, planting windows, and weather forecasts publish every Monday morning.</div>
      </div>
    </div>
  </div>

  <div class='wmw-op-footer'>
    <div class='wmw-stat-pill'>Dispatch: <strong>Live</strong></div>
    <div class='wmw-stat-pill'>Grid: <strong>MQT 100,56</strong></div>
  </div>
</div>")

  htmltools::HTML(html)
}
