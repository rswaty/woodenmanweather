source("R/constants.R")
source("R/interactive_charts.R")

wmw_ncei_daily <- function(start_date, end_date, station = wmw_station_id()) {
  query <- list(
    dataset = "daily-summaries",
    stations = station,
    startDate = format(start_date, "%Y-%m-%d"),
    endDate = format(end_date, "%Y-%m-%d"),
    dataTypes = "TMAX,TMIN,PRCP,SNOW",
    format = "json",
    units = "standard"
  )

  url <- httr::modify_url("https://www.ncei.noaa.gov/access/services/data/v1", query = query)
  payload <- wmw_get_json(url, timeout_sec = 120)

  if (length(payload) == 0) {
    return(data.frame(
      date = as.Date(character()),
      tmax_f = numeric(),
      tmin_f = numeric(),
      prcp_in = numeric(),
      snow_in = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    date = as.Date(payload$DATE),
    tmax_f = wmw_as_numeric(payload$TMAX),
    tmin_f = wmw_as_numeric(payload$TMIN),
    prcp_in = wmw_as_numeric(payload$PRCP),
    snow_in = wmw_as_numeric(payload$SNOW),
    stringsAsFactors = FALSE
  )
}

wmw_ncei_monthly_normals <- function(station = wmw_station_id()) {
  query <- list(
    dataset = "normals-monthly-1991-2020",
    stations = station,
    format = "json"
  )

  url <- httr::modify_url("https://www.ncei.noaa.gov/access/services/data/v1", query = query)
  payload <- wmw_get_json(url, timeout_sec = 120)

  month <- as.integer(payload$DATE)
  month_label <- format(as.Date(paste0("2020-", payload$DATE, "-01")), "%b")

  data.frame(
    month = month,
    month_label = month_label,
    tmax_f = wmw_as_numeric(payload$`MLY-TMAX-NORMAL`),
    tmin_f = wmw_as_numeric(payload$`MLY-TMIN-NORMAL`),
    tavg_f = (wmw_as_numeric(payload$`MLY-TMAX-NORMAL`) + wmw_as_numeric(payload$`MLY-TMIN-NORMAL`)) / 2,
    prcp_in = wmw_as_numeric(payload$`MLY-PRCP-NORMAL`),
    snow_in = wmw_as_numeric(payload$`MLY-SNOW-NORMAL`),
    stringsAsFactors = FALSE
  )
}

wmw_monthly_summary <- function(daily) {
  daily$month <- as.integer(format(daily$date, "%m"))
  daily$year <- as.integer(format(daily$date, "%Y"))
  daily$tavg_f <- (daily$tmax_f + daily$tmin_f) / 2

  tmax <- stats::aggregate(
    tmax_f ~ year + month,
    data = daily,
    FUN = function(x) mean(x, na.rm = TRUE),
    na.action = na.pass
  )
  tmax_max <- stats::aggregate(
    tmax_f ~ year + month,
    data = daily,
    FUN = function(x) max(x, na.rm = TRUE),
    na.action = na.pass
  )
  names(tmax_max)[names(tmax_max) == "tmax_f"] <- "tmax_max"

  tmin_min <- stats::aggregate(
    tmin_f ~ year + month,
    data = daily,
    FUN = function(x) min(x, na.rm = TRUE),
    na.action = na.pass
  )
  names(tmin_min)[names(tmin_min) == "tmin_f"] <- "tmin_min"

  tavg <- stats::aggregate(
    tavg_f ~ year + month,
    data = daily,
    FUN = function(x) mean(x, na.rm = TRUE),
    na.action = na.pass
  )
  prcp <- stats::aggregate(
    prcp_in ~ year + month,
    data = daily,
    FUN = function(x) sum(x, na.rm = TRUE),
    na.action = na.pass
  )
  snow <- stats::aggregate(
    snow_in ~ year + month,
    data = daily,
    FUN = function(x) sum(x, na.rm = TRUE),
    na.action = na.pass
  )

  monthly <- merge(tmax, tmax_max, by = c("year", "month"))
  monthly <- merge(monthly, tmin_min, by = c("year", "month"))
  monthly <- merge(monthly, tavg, by = c("year", "month"))
  monthly <- merge(monthly, prcp, by = c("year", "month"))
  monthly <- merge(monthly, snow, by = c("year", "month"))
  monthly
}

wmw_rolling_year_series <- function(
  end_date = Sys.Date(),
  station = wmw_station_id(),
  normals = NULL
) {
  start_date <- end_date - 365
  daily_cache <- file.path("data", "climate_daily.csv")
  normals_cache <- file.path("data", "climate_normals.csv")

  if (is.null(normals)) {
    if (file.exists(normals_cache)) {
      normals <- readr::read_csv(normals_cache, show_col_types = FALSE)
      if (!"tmax_f" %in% names(normals)) {
        normals <- wmw_ncei_monthly_normals(station)
        readr::write_csv(normals, normals_cache)
      }
    } else {
      normals <- wmw_ncei_monthly_normals(station)
      readr::write_csv(normals, normals_cache)
    }
  }

  daily <- if (file.exists(daily_cache) && file.size(daily_cache) > 0) {
    readr::read_csv(daily_cache, show_col_types = FALSE)
  } else {
    wmw_ncei_daily(start_date, end_date, station = station)
  }

  monthly <- wmw_monthly_summary(daily)

  monthly$month_label <- format(as.Date(paste0("2020-", monthly$month, "-01")), "%b")
  monthly <- merge(monthly, normals[, c("month", "tmax_f", "tavg_f", "prcp_in", "snow_in")], by = "month", suffixes = c("_obs", "_normal"), all.x = TRUE)
  names(monthly)[names(monthly) == "tmax_f_obs"] <- "tmax_obs"
  names(monthly)[names(monthly) == "tmax_f_normal"] <- "tmax_normal"
  names(monthly)[names(monthly) == "tavg_f_obs"] <- "tavg_obs"
  names(monthly)[names(monthly) == "prcp_in_obs"] <- "prcp_obs"
  names(monthly)[names(monthly) == "snow_in_obs"] <- "snow_obs"
  names(monthly)[names(monthly) == "tavg_f_normal"] <- "tavg_normal"
  names(monthly)[names(monthly) == "prcp_in_normal"] <- "prcp_normal"
  names(monthly)[names(monthly) == "snow_in_normal"] <- "snow_normal"

  # Ensure chronological order (from 12 months ago to present month)
  monthly <- monthly[order(monthly$year, monthly$month), ]

  # Keep exactly the rolling 12 months
  if (nrow(monthly) > 12) {
    monthly <- utils::tail(monthly, 12)
  }

  monthly$period_end <- end_date
  monthly
}

#' Rolling last-30-day precip and snowfall totals from daily cache / NCEI
wmw_last_30d_totals <- function(
  end_date = Sys.Date(),
  station = wmw_station_id()
) {
  start_date <- end_date - 29
  daily_cache <- file.path("data", "climate_daily.csv")

  daily <- if (file.exists(daily_cache) && file.size(daily_cache) > 0) {
    readr::read_csv(daily_cache, show_col_types = FALSE)
  } else {
    wmw_ncei_daily(start_date, end_date, station = station)
  }

  if (!"date" %in% names(daily) || nrow(daily) == 0) {
    return(list(prcp_in = 0, snow_in = 0, n_days = 0L))
  }

  daily$date <- as.Date(daily$date)
  recent <- daily[daily$date >= start_date & daily$date <= end_date, , drop = FALSE]

  list(
    prcp_in = sum(wmw_as_numeric(recent$prcp_in), na.rm = TRUE),
    snow_in = sum(wmw_as_numeric(recent$snow_in), na.rm = TRUE),
    n_days = nrow(recent)
  )
}

wmw_plot_rolling_metric <- function(series, obs_col, normal_col, title, ylab) {
  plot_df <- data.frame(
    month_label = factor(series$month_label, levels = unique(series$month_label)),
    observed = series[[obs_col]],
    normal = series[[normal_col]]
  )

  long_df <- tidyr::pivot_longer(
    plot_df,
    cols = c(observed, normal),
    names_to = "series",
    values_to = "value"
  )
  long_df$series <- factor(long_df$series, levels = c("observed", "normal"), labels = c("Observed", "1991–2020 normal"))

  ggplot2::ggplot(long_df, ggplot2::aes(x = month_label, y = value, color = series, group = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = c("#2f6f8f", "#3c7352")) +
    ggplot2::labs(title = title, x = NULL, y = ylab, color = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}
