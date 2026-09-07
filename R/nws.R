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
