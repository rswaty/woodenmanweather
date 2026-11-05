source("R/constants.R")

wmw_nws_context <- function() {
  coords <- wmw_marquette_coords()
  points_url <- paste0(
    "https://api.weather.gov/points/",
    coords$latitude,
    ",",
    coords$longitude
  )

  points <- wmw_get_json(points_url)

  list(
    points = points,
    forecast_url = points$properties$forecast,
    forecast_hourly_url = points$properties$forecastHourly,
    observation_stations_url = points$properties$observationStations
  )
}

wmw_nws_probability <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }

  if (is.atomic(x)) {
    return(wmw_as_numeric(x))
  }

  if (is.null(x$value)) {
    return(NA_real_)
  }

  wmw_as_numeric(x$value)
}

wmw_nws_forecast_periods <- function(context = wmw_nws_context()) {
  forecast <- wmw_get_json(context$forecast_url)
  periods <- forecast$properties$periods
  pop <- periods$probabilityOfPrecipitation
  pop_values <- if (is.data.frame(pop)) {
    wmw_as_numeric(pop$value)
  } else {
    vapply(pop, wmw_nws_probability, numeric(1))
  }

  data.frame(
    name = periods$name,
    start_time = periods$startTime,
    end_time = periods$endTime,
    is_daytime = periods$isDaytime,
    temperature = wmw_as_numeric(periods$temperature),
    temperature_unit = periods$temperatureUnit,
    short_forecast = periods$shortForecast,
    detailed_forecast = periods$detailedForecast,
    wind_speed = periods$windSpeed,
    wind_direction = periods$windDirection,
    probability_of_precipitation = pop_values,
    stringsAsFactors = FALSE
  )
}

wmw_nws_work_week <- function(context = wmw_nws_context()) {
  # Kept for callers; same as Mon–Fri of the current week.
  periods <- wmw_nws_forecast_periods(context)
  periods$start_date <- as.Date(lubridate::ymd_hms(periods$start_time, tz = "UTC"), tz = "America/Detroit")

  work_week_start <- lubridate::floor_date(
    lubridate::with_tz(Sys.time(), "America/Detroit"),
    unit = "week",
    week_start = 1
  )
  work_week_end <- work_week_start + lubridate::days(4)

  subset(
    periods,
    start_date >= as.Date(work_week_start) & start_date <= as.Date(work_week_end)
  )
}

#' Rolling N-day NWS forecast window starting today (America/Detroit).
wmw_nws_next_days <- function(context = wmw_nws_context(), days = 7) {
  periods <- wmw_nws_forecast_periods(context)
  periods$start_date <- as.Date(lubridate::ymd_hms(periods$start_time, tz = "UTC"), tz = "America/Detroit")

  today <- as.Date(lubridate::with_tz(Sys.time(), "America/Detroit"))
  end_day <- today + lubridate::days(as.integer(days) - 1L)

  out <- subset(
    periods,
    start_date >= today & start_date <= end_day
  )
  out[order(out$start_time), , drop = FALSE]
}

#' First non-empty string from an NWS list-column (jsonlite leaves these as lists).
wmw_alert_field <- function(col, n) {
  if (is.null(col) || length(col) == 0) return(rep(NA_character_, n))
  vapply(seq_len(n), function(i) {
    x <- if (is.list(col)) col[[i]] else col[i]
    x <- as.character(unlist(x, use.names = FALSE))
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) NA_character_ else x[[1]]
  }, character(1))
}

#' Human page for an alert: the issuing office's text product, or weather.gov.
wmw_nws_product_url <- function(awips, web = NA_character_) {
  fallback <- "https://www.weather.gov/"
  if (length(web) > 0 && !is.na(web[[1]]) && nzchar(web[[1]])) {
    fallback <- sub("^http://", "https://", web[[1]])
  }
  awips <- toupper(gsub("[^A-Za-z0-9]", "", paste(awips %||% "")))
  if (length(awips) == 0 || is.na(awips[[1]]) || !nzchar(awips[[1]]) || nchar(awips[[1]]) < 6) {
    return(fallback)
  }
  awips <- awips[[1]]
  office <- substr(awips, nchar(awips) - 2, nchar(awips))
  product <- substr(awips, 1, nchar(awips) - 3)
  if (!grepl("^[A-Z]{3}$", office) || !grepl("^[A-Z]{3,}$", product)) return(fallback)
  paste0(
    "https://forecast.weather.gov/product.php?site=", office,
    "&issuedby=", office,
    "&product=", product,
    "&format=CI&version=1&glossary=1"
  )
}

#' Active NWS alerts for Marquette plus the nearshore marine zones on either
#' side of the harbor (LSZ248 Huron Islands–Marquette, LSZ249 Marquette–Munising).
#' Attribute `fetched` is FALSE only when every request failed.
wmw_nws_alerts <- function(zones = c("LSZ248", "LSZ249")) {
  coords <- wmw_marquette_coords()
  urls <- c(
    paste0("https://api.weather.gov/alerts/active?point=", coords$latitude, ",", coords$longitude),
    if (length(zones) > 0) paste0("https://api.weather.gov/alerts/active?zone=", paste(zones, collapse = ","))
  )

  empty <- data.frame(
    event = character(), headline = character(), severity = character(),
    onset = character(), ends = character(), url = character(),
    stringsAsFactors = FALSE
  )

  fetched <- FALSE
  pieces <- lapply(urls, function(url) {
    payload <- tryCatch(wmw_get_json(url), error = function(e) NULL)
    if (is.null(payload)) return(empty)
    fetched <<- TRUE
    props <- payload$features$properties
    if (is.null(props) || !is.data.frame(props) || nrow(props) == 0) return(empty)
    n <- nrow(props)
    ends <- as.character(props$ends %||% rep(NA_character_, n))
    expires <- as.character(props$expires %||% rep(NA_character_, n))
    ends[is.na(ends) | !nzchar(ends)] <- expires[is.na(ends) | !nzchar(ends)]
    awips_col <- if (is.data.frame(props$parameters)) props$parameters$AWIPSidentifier else NULL
    awips <- wmw_alert_field(awips_col, n)
    web <- wmw_alert_field(props$web, n)
    data.frame(
      event = as.character(props$event %||% ""),
      headline = as.character(props$headline %||% ""),
      severity = as.character(props$severity %||% ""),
      onset = as.character(props$onset %||% rep(NA_character_, n)),
      ends = ends,
      url = mapply(wmw_nws_product_url, awips, web, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  })

  alerts <- do.call(rbind, pieces)
  if (nrow(alerts) == 0) {
    attr(empty, "fetched") <- fetched
    return(empty)
  }
  # Same event from the point and a marine zone: keep the office product page.
  product_page <- startsWith(alerts$url, "https://forecast.weather.gov/")
  alerts <- alerts[order(alerts$event, !product_page), , drop = FALSE]
  alerts <- alerts[!duplicated(alerts$event), , drop = FALSE]
  attr(alerts, "fetched") <- fetched
  alerts
}

wmw_parse_nws_time <- function(x) {
  x <- as.character(x)
  out <- rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), length(x))
  ok <- !is.na(x) & nzchar(x)
  if (any(ok)) out[ok] <- lubridate::as_datetime(x[ok])
  out
}

#' Warnings, advisories, and alerts that overlap the local day.
#' Watches and statements stay off this list. Attribute `fetched` is FALSE
#' when the NWS could not be reached.
wmw_warnings_today <- function(date = Sys.Date(), zones = c("LSZ248", "LSZ249")) {
  alerts <- wmw_nws_alerts(zones)
  fetched <- isTRUE(attr(alerts, "fetched"))
  empty <- alerts[0, , drop = FALSE]
  attr(empty, "fetched") <- fetched
  if (!fetched || nrow(alerts) == 0) return(empty)

  alerts <- alerts[grepl("(Warning|Advisory|Alert)$", alerts$event), , drop = FALSE]
  if (nrow(alerts) == 0) return(empty)

  day_start <- as.POSIXct(format(as.Date(date)), tz = "America/Detroit")
  day_end <- day_start + 86400
  onset <- wmw_parse_nws_time(alerts$onset)
  ends <- wmw_parse_nws_time(alerts$ends)
  overlaps <- (is.na(onset) | onset < day_end) & (is.na(ends) | ends > day_start)
  alerts <- alerts[overlaps, , drop = FALSE]
  if (nrow(alerts) == 0) return(empty)

  rank <- c(Extreme = 1L, Severe = 2L, Moderate = 3L, Minor = 4L, Unknown = 5L)
  alerts$.rank <- unname(rank[alerts$severity])
  alerts$.rank[is.na(alerts$.rank)] <- 5L
  alerts$.kind <- ifelse(grepl("Warning$", alerts$event), 1L,
                    ifelse(grepl("Alert$", alerts$event), 2L, 3L))
  alerts <- alerts[order(alerts$.kind, alerts$.rank, alerts$event), , drop = FALSE]
  alerts$.rank <- NULL
  alerts$.kind <- NULL
  attr(alerts, "fetched") <- TRUE
  alerts
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

wmw_nws_current_summary <- function(context = wmw_nws_context()) {
  stations <- wmw_get_json(context$observation_stations_url)
  station_url <- stations$features$id[[1]]
  latest <- wmw_get_json(paste0(station_url, "/observations/latest"))

  props <- latest$properties
  temp_f <- if (is.null(props$temperature$value)) {
    NA_real_
  } else {
    if (identical(props$temperature$unitCode, "wmoUnit:degC")) {
      (props$temperature$value * 9 / 5) + 32
    } else {
      wmw_as_numeric(props$temperature$value)
    }
  }

  list(
    station = stations$features$properties$name[[1]],
    observed_at = props$timestamp,
    temperature_f = temp_f,
    description = props$textDescription %||% "No observation text available.",
    wind_speed = props$windSpeed$value,
    wind_direction = props$windDirection$value
  )
}

#' Next ~24h Quantitative Precipitation Forecast (inches) from NWS grid
wmw_nws_qpf_24h_inches <- function(context = wmw_nws_context()) {
  grid_url <- sub("/forecast$", "", context$forecast_url %||% "")
  if (!nzchar(grid_url)) {
    return(0)
  }

  grid <- tryCatch(wmw_get_json(grid_url), error = function(e) NULL)
  if (is.null(grid)) {
    return(0)
  }

  qpf <- grid$properties$quantitativePrecipitation$values
  if (is.null(qpf) || !is.data.frame(qpf) || nrow(qpf) == 0) {
    return(0)
  }

  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  end_utc <- now_utc + 24 * 3600

  starts <- as.POSIXct(
    sub("/.*$", "", qpf$validTime),
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )
  # Include bins that started recently (overlap with "now") through next 24h
  keep <- !is.na(starts) & starts >= (now_utc - 6 * 3600) & starts < end_utc
  mm <- sum(wmw_as_numeric(qpf$value[keep]), na.rm = TRUE)
  if (is.na(mm) || mm < 0) mm <- 0

  round(mm / 25.4, 2)
}
