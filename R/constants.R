# Wooden Man Weather — data helpers for Marquette, MI (USW00094850)

#' HTTP user agent required by NWS and recommended for NCEI.
wmw_user_agent <- function() {
  "WoodenManWeather/0.1 (https://github.com/rswaty/woodenmanweather; contact: hello@woodenmanweather.example)"
}

wmw_marquette_coords <- function() {
  list(latitude = 46.5436, longitude = -87.3954)
}

wmw_station_id <- function() {
  "USW00094850"
}

wmw_normals_start_year <- function() {
  1991L
}

wmw_normals_end_year <- function() {
  2020L
}

wmw_get_json <- function(url, timeout_sec = 60) {
  response <- httr::GET(
    url,
    httr::add_headers(`User-Agent` = wmw_user_agent()),
    httr::timeout(timeout_sec)
  )

  if (httr::status_code(response) != 200) {
    stop(
      "Request failed (HTTP ",
      httr::status_code(response),
      "): ",
      url,
      call. = FALSE
    )
  }

  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
}

wmw_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

wmw_format_f <- function(x, digits = 0) {
  if (is.na(x)) {
    return("—")
  }

  paste0(format(round(x, digits), nsmall = digits), "°F")
}

wmw_format_in <- function(x, digits = 2) {
  if (is.na(x)) {
    return("—")
  }

  paste0(format(round(x, digits), nsmall = digits), " in")
}

wmw_format_pct <- function(x, digits = 0) {
  if (is.na(x)) {
    return("—")
  }

  paste0(format(round(x, digits), nsmall = digits), "%")
}

wmw_format_dt <- function(x, tz = "America/Detroit") {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("—")
  }

  format(
    lubridate::with_tz(lubridate::ymd_hms(x, tz = "UTC"), tzone = tz),
    "%b %d, %Y %I:%M %p %Z"
  )
}
