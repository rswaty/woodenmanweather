source("R/constants.R")

wmw_monday_tasks <- function(
  work_week = NULL,
  current = NULL,
  rolling = NULL,
  today = lubridate::with_tz(Sys.time(), "America/Detroit")
) {
  tasks <- list()

  month <- lubridate::month(today)
  weekday <- lubridate::wday(today, week_start = 1)

  if (weekday != 1) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Briefing cadence",
      detail = "The Wooden Man work-week briefing is written for Monday mornings. Check back at the start of the week for fresh tasks."
    )
  }

  if (month %in% c(4, 5)) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Soil and early crops",
      detail = "If beds are workable and hard frosts are not in the near-term forecast, cool-season crops such as peas and onions can go in. Keep row cover handy for late cold snaps."
    )
  }

  if (month %in% c(4, 5, 10)) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Tire changeover",
      detail = "Sustained overnight lows above freezing and a dry work-week forecast are a practical window to swap winter tires for all-season or summer rubber."
    )
  }

  if (month %in% c(10, 11, 3, 4)) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Snow tires and traction",
      detail = "If wet snow or refreeze is in the forecast, delay removal of winter tires and keep ice scrapers and traction aids in the vehicle."
    )
  }

  if (month %in% c(6, 7, 8)) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Garden succession",
      detail = "Warm-season transplants need hardening off and steady watering. Plan succession plantings before the growing season compresses in fall."
    )
  }

  if (month %in% c(9, 10)) {
    tasks[[length(tasks) + 1]] <- list(
      title = "Winter prep",
      detail = "Service furnaces, inspect roof edges, and stage snow tools before the first lasting lake-effect cycle."
    )
  }

  if (!is.null(work_week) && nrow(work_week) > 0) {
    precip_chance <- max(work_week$probability_of_precipitation, na.rm = TRUE)
    if (is.finite(precip_chance) && precip_chance >= 50) {
      tasks[[length(tasks) + 1]] <- list(
        title = "Wet-week logistics",
        detail = "Precipitation chances climb mid-week. Move outdoor deliveries under cover and plan for muddy job sites away from the lakeshore."
      )
    }

    daytime_temps <- work_week$temperature[work_week$is_daytime]
    if (length(daytime_temps) > 0 && max(daytime_temps, na.rm = TRUE) >= 70) {
      tasks[[length(tasks) + 1]] <- list(
        title = "Warm-week outdoor block",
        detail = "Daytime highs near or above 70°F favor exterior paint, roof patch work, and dock prep when winds stay manageable."
      )
    }
  }

  if (!is.null(rolling) && nrow(rolling) > 0) {
    latest <- rolling[which.max(rolling$year * 100 + rolling$month), ]
    if (!is.na(latest$snow_obs) && !is.na(latest$snow_normal) && latest$snow_obs > latest$snow_normal * 1.2) {
      tasks[[length(tasks) + 1]] <- list(
        title = "Above-normal snow month",
        detail = "Recent snowfall is running above the 1991–2020 normal. Clear roof valleys and keep intake vents open after heavy events."
      )
    }
  }

  if (length(tasks) == 0) {
    tasks[[1]] <- list(
      title = "Steady week",
      detail = "No seasonal triggers fired this week. Use the dashboard charts to compare the rolling year against Marquette normals."
    )
  }

  tasks
}

wmw_tasks_dataframe <- function(tasks) {
  data.frame(
    task = vapply(tasks, function(x) x$title, character(1)),
    guidance = vapply(tasks, function(x) x$detail, character(1)),
    stringsAsFactors = FALSE
  )
}
