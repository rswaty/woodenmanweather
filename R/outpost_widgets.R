# Wooden Man Weather — Outpost Top Row Widgets (Option 1: Field Slate & Forest Pine)
# Provides:
# 1. Global Human Thermometer (Instrument Readout + Percentile Bar + Stats)
# 2. Big Lake and Flannels Gauge (Lake, Breeze, Flannel Index, Daylight)

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

#' Latest Lake Superior buoy readings.
#' First choice is the Superior Watershed Partnership Marquette buoy (off Presque
#' Isle) via the GLOS public ERDDAP, which can lag a day or more, so its reading
#' time is always shown. Fallbacks are NOAA NDBC buoys with a reading in the last
#' 12 hours. All buoys are pulled for winter (~Nov to May).
wmw_lake_buoy_stations <- function() {
  data.frame(
    id = c("45211", "45025", "45023"),
    name = c("Grand Island buoy", "South Entry buoy", "North Entry buoy"),
    stringsAsFactors = FALSE
  )
}

#' Marquette Spotter buoy (GLOS obs_139). Skips readings whose QARTOD
#' aggregate flag is 4 (fail); 1 pass, 2 not evaluated, 3 suspect are kept.
wmw_marquette_buoy_latest <- function(max_age_hours = 72) {
  url <- paste0(
    "https://seagull-erddap.glos.org/erddap/tabledap/obs_139.csv?",
    "time,sea_surface_temperature,sea_surface_temperature_aggregate_test,",
    "sea_surface_wave_significant_height,sea_surface_wave_significant_height_aggregate_test",
    "&time%3E=now-", ceiling(max_age_hours / 24), "days"
  )
  txt <- tryCatch({
    r <- httr::GET(url, httr::add_headers(`User-Agent` = wmw_user_agent()), httr::timeout(30))
    if (httr::status_code(r) != 200) NULL else httr::content(r, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)
  if (is.null(txt)) return(NULL)

  lines <- strsplit(txt, "\n", fixed = FALSE)[[1]]
  if (length(lines) < 3) return(NULL)
  d <- utils::read.csv(text = paste(lines[-2], collapse = "\n"), stringsAsFactors = FALSE)
  when <- as.POSIXct(d$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  fresh <- !is.na(when) & when >= Sys.time() - max_age_hours * 3600

  pick <- function(val, flag) {
    v <- wmw_as_numeric(val)
    f <- wmw_as_numeric(flag)
    ok <- which(fresh & !is.na(v) & (is.na(f) | f != 4))
    if (length(ok) == 0) return(NULL)
    i <- ok[which.max(when[ok])]
    list(value = v[i], time = when[i])
  }
  wtmp <- pick(d$sea_surface_temperature, d$sea_surface_temperature_aggregate_test)
  if (!is.null(wtmp)) wtmp$value <- wtmp$value - 273.15
  list(
    wtmp_c = wtmp,
    wvht_m = pick(d$sea_surface_wave_significant_height, d$sea_surface_wave_significant_height_aggregate_test)
  )
}

wmw_ndbc_latest <- function(station_id, max_age_hours = 12) {
  url <- paste0("https://www.ndbc.noaa.gov/data/realtime2/", station_id, ".txt")
  txt <- tryCatch({
    r <- httr::GET(url, httr::add_headers(`User-Agent` = wmw_user_agent()), httr::timeout(20))
    if (httr::status_code(r) != 200) NULL else httr::content(r, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)
  if (is.null(txt)) return(NULL)

  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  header <- strsplit(trimws(sub("^#", "", lines[1])), "\\s+")[[1]]
  rows <- utils::head(lines[!startsWith(lines, "#") & nzchar(trimws(lines))], 60)
  if (length(rows) == 0) return(NULL)
  parts <- strsplit(trimws(rows), "\\s+")
  parts <- parts[lengths(parts) == length(header)]
  if (length(parts) == 0) return(NULL)
  m <- do.call(rbind, parts)
  colnames(m) <- header

  when <- as.POSIXct(
    sprintf("%s-%s-%s %s:%s", m[, "YY"], m[, 2], m[, "DD"], m[, "hh"], m[, "mm"]),
    format = "%Y-%m-%d %H:%M", tz = "UTC"
  )
  fresh <- !is.na(when) & when >= Sys.time() - max_age_hours * 3600

  pick <- function(col) {
    if (!col %in% colnames(m)) return(NULL)
    v <- wmw_as_numeric(m[, col])
    i <- which(fresh & !is.na(v))[1]
    if (is.na(i)) NULL else list(value = v[i], time = when[i])
  }
  list(wtmp_c = pick("WTMP"), wvht_m = pick("WVHT"))
}

wmw_lake_buoy <- function() {
  stations <- wmw_lake_buoy_stations()
  out <- list(water_f = NA_real_, water_src = NA_character_, water_time = NULL,
              wave_ft = NA_real_, wave_src = NA_character_, wave_time = NULL)

  take <- function(out, obs, name) {
    if (is.null(obs)) return(out)
    if (is.na(out$water_f) && !is.null(obs$wtmp_c)) {
      out$water_f <- obs$wtmp_c$value * 9 / 5 + 32
      out$water_src <- name
      out$water_time <- obs$wtmp_c$time
    }
    if (is.na(out$wave_ft) && !is.null(obs$wvht_m)) {
      out$wave_ft <- obs$wvht_m$value * 3.28084
      out$wave_src <- name
      out$wave_time <- obs$wvht_m$time
    }
    out
  }

  out <- take(out, wmw_marquette_buoy_latest(), "Marquette buoy")
  for (k in seq_len(nrow(stations))) {
    if (!is.na(out$water_f) && !is.na(out$wave_ft)) break
    out <- take(out, wmw_ndbc_latest(stations$id[k]), stations$name[k])
  }
  out
}

#' "today 6:56 AM" / "yesterday 7:38 PM" / "Fri 7:38 PM" in Marquette time.
wmw_when <- function(x, tz = "America/Detroit") {
  local <- lubridate::with_tz(x, tz)
  d <- as.Date(local, tz = tz)
  today <- as.Date(lubridate::with_tz(Sys.time(), tz), tz = tz)
  day <- if (d == today) "today" else if (d == today - 1) "yesterday" else format(local, "%a")
  paste(day, wmw_clock(x, tz))
}

wmw_clock <- function(x, tz = "America/Detroit") {
  trimws(format(lubridate::with_tz(x, tz), "%l:%M %p"))
}

#' Sunrise / sunset for Marquette (NOAA solar equations, standard refraction).
wmw_sun_times <- function(date = Sys.Date(), tz = "America/Detroit") {
  coords <- wmw_marquette_coords()
  lat_r <- coords$latitude * pi / 180
  doy <- as.integer(format(date, "%j"))
  gamma <- 2 * pi / 365 * (doy - 1)
  eqtime <- 229.18 * (0.000075 + 0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
    0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma))
  decl <- 0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) -
    0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma) -
    0.002697 * cos(3 * gamma) + 0.00148 * sin(3 * gamma)
  cos_ha <- cos(90.833 * pi / 180) / (cos(lat_r) * cos(decl)) - tan(lat_r) * tan(decl)
  ha <- acos(max(-1, min(1, cos_ha))) * 180 / pi

  midnight_utc <- as.POSIXct(format(as.Date(date)), tz = "UTC")
  rise <- midnight_utc + (720 - 4 * (coords$longitude + ha) - eqtime) * 60
  set <- midnight_utc + (720 - 4 * (coords$longitude - ha) - eqtime) * 60
  list(
    sunrise = rise,
    sunset = set,
    day_len_sec = as.numeric(difftime(set, rise, units = "secs"))
  )
}

#' Daylight length, day-over-day change, and sunrise/sunset strings.
#' Uses official USNO sunrise/sunset when `usno` has them; the NOAA equations
#' (~2 min error) are the fallback and always supply the day-over-day change.
wmw_calc_daylight_arc <- function(date = Sys.Date(), usno = NULL) {
  today <- wmw_sun_times(date)
  tomorrow <- wmw_sun_times(as.Date(date) + 1)
  diff_sec <- round(tomorrow$day_len_sec - today$day_len_sec)

  if (!is.null(usno) && !is.null(usno$sunrise) && !is.null(usno$sunset)) {
    today$sunrise <- usno$sunrise
    today$sunset <- usno$sunset
    today$day_len_sec <- as.numeric(difftime(usno$sunset, usno$sunrise, units = "secs"))
  }

  len_min <- round(today$day_len_sec / 60)
  diff_mins_abs <- abs(diff_sec) %/% 60
  diff_sec_rem <- abs(diff_sec) %% 60
  sign_str <- if (diff_sec >= 0) "+" else "-"

  list(
    length_str = paste0(len_min %/% 60, "h ", len_min %% 60, "m"),
    change_str = paste0(sign_str, diff_mins_abs, "m ", diff_sec_rem, "s/day"),
    is_decreasing = diff_sec < 0,
    sunrise_str = wmw_clock(today$sunrise),
    sunset_str = wmw_clock(today$sunset)
  )
}

#' Traditional full moon name; Harvest Moon is the full moon nearest the
#' autumn equinox, and Hunter's Moon is the one after it.
wmw_full_moon_name <- function(d) {
  d <- as.Date(d)
  mon <- as.integer(format(d, "%m"))
  day <- as.integer(format(d, "%d"))
  if (mon == 9 && day >= 8) return("Harvest Moon")
  if (mon == 10 && day <= 7) return("Harvest Moon")
  if (mon == 10 || (mon == 11 && day <= 6)) return("Hunter's Moon")
  c("Wolf Moon", "Snow Moon", "Worm Moon", "Pink Moon", "Flower Moon", "Strawberry Moon",
    "Buck Moon", "Sturgeon Moon", "Corn Moon", "Hunter's Moon", "Beaver Moon", "Cold Moon")[mon]
}

#' Moon phase, illumination, moonrise, next full moon, and official
#' sunrise/sunset from the U.S. Naval Observatory. NULL if the service is down.
wmw_moon_info <- function(date = Sys.Date()) {
  coords <- wmw_marquette_coords()
  day_url <- paste0(
    "https://aa.usno.navy.mil/api/rstt/oneday?date=", format(as.Date(date)),
    "&coords=", coords$latitude, ",", coords$longitude, "&tz=-5&dst=true"
  )
  phase_url <- paste0(
    "https://aa.usno.navy.mil/api/moon/phases/date?date=", format(as.Date(date)), "&nump=4"
  )
  d <- tryCatch(wmw_get_json(day_url, timeout_sec = 30)$properties$data, error = function(e) NULL)
  if (is.null(d) || is.null(d$curphase)) return(NULL)

  to_time <- function(t) {
    if (length(t) == 0 || is.na(t[1])) return(NULL)
    hm <- sub("\\s.*$", "", trimws(t[1]))
    as.POSIXct(paste(format(as.Date(date)), hm), format = "%Y-%m-%d %H:%M", tz = "America/Detroit")
  }
  md <- d$moondata
  sd <- d$sundata
  moonrise <- if (is.data.frame(md)) to_time(md$time[md$phen == "Rise"]) else NULL
  rise <- if (is.null(moonrise)) NA_character_ else wmw_clock(moonrise)
  sunrise <- if (is.data.frame(sd)) to_time(sd$time[sd$phen == "Rise"]) else NULL
  sunset <- if (is.data.frame(sd)) to_time(sd$time[sd$phen == "Set"]) else NULL

  next_full <- NULL
  ph <- tryCatch(wmw_get_json(phase_url, timeout_sec = 30)$phasedata, error = function(e) NULL)
  if (is.data.frame(ph)) {
    full <- ph[ph$phase == "Full Moon", , drop = FALSE]
    if (nrow(full) > 0) next_full <- as.Date(sprintf("%04d-%02d-%02d", full$year[1], full$month[1], full$day[1]))
  }
  cp <- d$closestphase
  if (!is.null(cp) && identical(cp$phase, "Full Moon")) {
    cp_date <- as.Date(sprintf("%04d-%02d-%02d", cp$year, cp$month, cp$day))
    if (cp_date == as.Date(date)) next_full <- cp_date
  }

  list(
    phase = d$curphase,
    illum = d$fracillum,
    rise = rise,
    next_full = next_full,
    sunrise = sunrise,
    sunset = sunset
  )
}

#' Flannel Index Rating
wmw_calc_flannel_index <- function(today_high) {
  if (is.null(today_high) || is.na(today_high)) today_high <- 70

  if (today_high >= 78) {
    list(level = "Level 1", title = "No Flannel", desc = "Windows open · T-shirt weather", color = "#fbbf24")
  } else if (today_high >= 65) {
    list(level = "Level 2", title = "Light Flannel", desc = "Windows cracked · Morning chill", color = "#34d399")
  } else if (today_high >= 50) {
    list(level = "Level 3", title = "Full Flannel", desc = "Layer up · Jacket for the evening", color = "#38bdf8")
  } else if (today_high >= 32) {
    list(level = "Level 4", title = "Flannel & Wool Socks", desc = "Hat and gloves by the door", color = "#c084fc")
  } else if (today_high >= 15) {
    list(level = "Level 5", title = "Flannel-Lined Everything", desc = "Long johns · Keep the kettle on", color = "#f87171")
  } else {
    list(level = "Level 6", title = "Polar Vortex", desc = "Every layer you own · Check on the neighbors", color = "#fb7185")
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

#' Rain Gauge / 24-Hour Soak (Idea A)
wmw_calc_rain_gauge <- function(inches_24h) {
  if (is.null(inches_24h) || is.na(inches_24h)) inches_24h <- 0
  inches_24h <- max(0, as.numeric(inches_24h))

  if (inches_24h < 0.05) {
    list(
      amount_str = "Trace / Dry",
      desc = "Dust on the gravel roads",
      color = "#fbbf24"
    )
  } else if (inches_24h < 0.35) {
    list(
      amount_str = paste0(format(round(inches_24h, 1), nsmall = 1), " in Expected"),
      desc = "Good for the gardens & root cellar",
      color = "#34d399"
    )
  } else if (inches_24h < 0.75) {
    list(
      amount_str = paste0(format(round(inches_24h, 1), nsmall = 1), " in Expected"),
      desc = "Steady soaker · Keep eaves clear",
      color = "#38bdf8"
    )
  } else {
    list(
      amount_str = paste0(format(round(inches_24h, 1), nsmall = 1), " in Expected"),
      desc = "Heavy soak · Sump pumps working",
      color = "#60a5fa"
    )
  }
}

#' Rain Barrel / Soil Moisture Index (Idea D)
wmw_calc_soil_moisture_index <- function(inches_24h) {
  if (is.null(inches_24h) || is.na(inches_24h)) inches_24h <- 0
  inches_24h <- max(0, as.numeric(inches_24h))

  if (inches_24h < 0.05) {
    list(
      level = "Level 1",
      title = "Dust",
      desc = "Ground dry · Fire risk creeping up",
      color = "#fbbf24"
    )
  } else if (inches_24h < 0.35) {
    list(
      level = "Level 2",
      title = "Good Soak",
      desc = "Garden damp · Rain barrels topped off",
      color = "#34d399"
    )
  } else if (inches_24h < 0.75) {
    list(
      level = "Level 3",
      title = "Two-Track Mud",
      desc = "Standing puddles · Saturated woods",
      color = "#38bdf8"
    )
  } else {
    list(
      level = "Level 4",
      title = "Creek Rise",
      desc = "Basement drains · Avoid dirt roads",
      color = "#60a5fa"
    )
  }
}

#' Detroit-local Last Updated stamp (no leading spaces)
wmw_format_updated_stamp <- function(when = Sys.time()) {
  update_time <- lubridate::with_tz(as.POSIXct(when), "America/Detroit")
  stamp <- format(update_time, "%a, %b %e · %l:%M %p %Z")
  gsub("\\s+", " ", trimws(stamp))
}

#' CARD 1: Global Human Thermometer Widget
wmw_card_human_thermometer <- function(today_high) {
  if (is.null(today_high) || is.na(today_high)) today_high <- 73
  ht <- wmw_calc_human_thermometer(today_high)

  accent_color <- if (ht$is_warmer_than_half) "#f59e0b" else "#38bdf8"

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
  # Global median sits at the 50th percentile of the human distribution
  median_pct <- 50

  html <- paste0(
"<div class='wmw-outpost-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Global Human Thermometer</span>
  </div>
  <div class='wmw-op-body'>
    <div class='wmw-human-headline'>", headline_html, "</div>
    <div class='wmw-human-sub'>", subtext_html, "</div>
    
    <div class='wmw-earth-bar-shell'>
      <div class='wmw-earth-bar-track'>
        <div class='wmw-earth-bar-median' style='left: ", median_pct, "%;' title='Global Median ", ht$global_median, "°F'></div>
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
    <div class='wmw-stat-pill'>Median <strong>", ht$global_median, "°F</strong></div>
    <div class='wmw-stat-pill'>Coldest <strong>-62°F Vostok</strong></div>
    <div class='wmw-stat-pill'>Warmest <strong>114°F Kuwait</strong></div>
  </div>
</div>")

  htmltools::HTML(html)
}

#' CARD 2: Big Lake and Flannels Gauge Widget
wmw_card_lake_flannels <- function(today_high, forecast_df, date = Sys.Date(),
                                   context = NULL, inches_24h = NULL) {
  buoy <- wmw_lake_buoy()
  moon <- wmw_moon_info(date)
  daylight <- wmw_calc_daylight_arc(date, usno = moon)
  flannel <- wmw_calc_flannel_index(today_high)

  # Extract first period wind
  first_wind_dir <- if (nrow(forecast_df) > 0) forecast_df$wind_direction[1] else "S"
  first_wind_speed <- if (nrow(forecast_df) > 0) forecast_df$wind_speed[1] else "10 mph"
  surf <- wmw_calc_lake_breeze(first_wind_dir, first_wind_speed)

  daylight_color <- if (daylight$is_decreasing) "#fca5a5" else "#86efac"
  esc <- htmltools::htmlEscape

  gauge <- function(label, value_html) {
    paste0(
      "<div class='wmw-gauge-item'><span class='wmw-gauge-label'>", label,
      "</span><span class='wmw-gauge-val'>", value_html, "</span></div>"
    )
  }

  # Lake water: real buoy readings, or an honest off-season note
  if (!is.na(buoy$water_f)) {
    wave_html <- if (!is.na(buoy$wave_ft)) {
      paste0(" &nbsp;·&nbsp; <strong style='color: #7dd3fc;'>", format(round(buoy$wave_ft, 1), nsmall = 1), " ft</strong> <span class='wmw-gauge-sub'>waves</span>")
    } else {
      ""
    }
    note <- paste0(buoy$water_src, " · reading from ", wmw_when(buoy$water_time))
    if (!is.na(buoy$wave_ft) && !identical(buoy$wave_src, buoy$water_src)) {
      note <- paste0(note, " · waves: ", buoy$wave_src, ", ", wmw_when(buoy$wave_time))
    }
    water_html <- paste0(
      "<strong style='color: #38bdf8;'>", round(buoy$water_f), "°F</strong> <span class='wmw-gauge-sub'>water</span>",
      wave_html,
      "<span class='wmw-gauge-note'>", esc(note), "</span>"
    )
  } else {
    water_html <- "<strong style='color: #94a3b8;'>Buoys out for the season</strong> <span class='wmw-gauge-sub'>— readings resume in spring</span>"
  }

  moon_gauge <- ""
  if (!is.null(moon)) {
    bits <- c(
      if (!is.na(moon$rise)) paste0("Rises ", moon$rise),
      if (!is.null(moon$next_full)) {
        if (moon$next_full == as.Date(date)) {
          paste0("Full tonight (", wmw_full_moon_name(moon$next_full), ")")
        } else {
          paste0("Next full ", format(moon$next_full, "%b "), as.integer(format(moon$next_full, "%d")),
                 " (", wmw_full_moon_name(moon$next_full), ")")
        }
      }
    )
    moon_gauge <- gauge("Moon", paste0(
      "<strong style='color: #e2e8f0;'>", esc(moon$phase), "</strong> <span class='wmw-gauge-sub'>· ", esc(moon$illum), " lit</span>",
      if (length(bits) > 0) paste0("<span class='wmw-gauge-note'>", esc(paste(bits, collapse = " · ")), "</span>") else ""
    ))
  }

  rows <- c(
    gauge("Lake Superior Water", water_html),
    gauge("Lake Breeze / Surf", paste0("<strong style='color: ", surf$color, ";'>", surf$state, "</strong> <span class='wmw-gauge-sub'>— ", surf$desc, "</span>")),
    gauge("Flannel Index", paste0("<strong style='color: ", flannel$color, ";'>", flannel$level, ": ", flannel$title, "</strong> <span class='wmw-gauge-sub'>— ", flannel$desc, "</span>")),
    gauge("Daylight", paste0(
      "<strong style='color: #ffffff;'>", daylight$length_str, "</strong> <span class='wmw-gauge-sub' style='color: ", daylight_color, "; font-weight: 600;'>&nbsp;(", daylight$change_str, ")</span>",
      "<span class='wmw-gauge-note'>Sunrise ", daylight$sunrise_str, " · Sunset ", daylight$sunset_str, "</span>"
    )),
    moon_gauge
  )

  html <- paste0(
"<div class='wmw-outpost-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Big Lake and Flannels</span>
  </div>
  <div class='wmw-op-body'>
    <div class='wmw-gauge-list'>
      ", paste(rows[nzchar(rows)], collapse = "\n      "), "
    </div>
  </div>
</div>")

  htmltools::HTML(html)
}

#' CARD 3: Left Column Logo Placeholder Widget
#' @param blurbs Character vector of short dispatch paragraphs (edited in index.qmd).
wmw_card_sidebar_dispatch <- function(blurbs = character()) {
  update_str <- wmw_format_updated_stamp()

  blurb_html <- ""
  if (length(blurbs) > 0) {
    blurbs <- blurbs[!is.na(blurbs) & nzchar(trimws(blurbs))]
    if (length(blurbs) > 0) {
      blurb_html <- paste0(
        "<div class='wmw-dispatch-blurb'>",
        htmltools::htmlEscape(blurbs),
        "</div>",
        collapse = "\n    "
      )
    }
  }

  html <- paste0(
"<div class='wmw-outpost-card wmw-sidebar-card'>
  <div class='wmw-op-header'>
    <span class='wmw-op-title'>Outpost Dispatch</span>
  </div>
  
  <div class='wmw-op-body' style='justify-content: flex-start; gap: 12px;'>
    <div class='wmw-logo-box'>
      <img class='wmw-logo-img' src='images/logo.jpg' alt='Wooden Man Weather logo'>
      <div class='wmw-logo-credit'>Logo design by Leo Barch</div>
      <div class='wmw-logo-sub'>Marquette · Lake Superior Outpost</div>
    </div>
  </div>

  <div class='wmw-op-footer wmw-sidebar-footer'>
    <div class='wmw-stat-pill'>Last Updated: <strong>", update_str, "</strong></div>
    <div class='wmw-stat-pill'>Station: <strong>Marquette 46.54°N</strong></div>
    <div class='wmw-update-note'>Forecast auto-updates ~5am / 5pm Eastern</div>
    ", blurb_html, "
  </div>
</div>")

  htmltools::HTML(html)
}
