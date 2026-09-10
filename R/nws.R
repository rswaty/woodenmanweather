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

wmw_nws_alerts <- function() {
  coords <- wmw_marquette_coords()
  url <- paste0(
    "https://api.weather.gov/alerts/active?point=",
    coords$latitude,
    ",",
    coords$longitude
  )

  payload <- tryCatch(
    wmw_get_json(url),
    error = function(e) NULL
  )

  features <- payload$features
  empty_features <- is.null(features) ||
    (is.data.frame(features) && nrow(features) == 0) ||
    length(features) == 0

  if (is.null(payload) || empty_features) {
    return(data.frame(
      event = character(),
      headline = character(),
      severity = character(),
      ends = character(),
      stringsAsFactors = FALSE
    ))
  }

  props <- payload$features$properties
  data.frame(
    event = props$event %||% "",
    headline = props$headline %||% "",
    severity = props$severity %||% "",
    ends = props$ends %||% "",
    stringsAsFactors = FALSE
  )
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
