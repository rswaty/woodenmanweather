source("R/constants.R")

# =============================================================================
# MONDAY EDIT HOOK — change tasks here when you hand-tune the weekly outlook
# File: R/tasks.R  (this block only, unless you are changing auto rules below)
# After editing: quarto render monday.qmd  (or push and let Actions rebuild)
#
# Put 0–3 hand-written tasks here. They appear first and are never dropped
# by the auto cap. Leave as list() to rely on weather/season auto tasks only.
#
# Example:
# wmw_manual_monday_tasks <- list(
#   list(
#     title = "Friday is the gift",
#     detail = "Highs near 80 and sunny — hammock or Presque Isle rocks while it lasts."
#   )
# )
# =============================================================================
wmw_manual_monday_tasks <- list()

#' Parse a rough max mph from NWS wind_speed strings like "10 to 15 mph".
wmw_parse_wind_mph <- function(wind_speed) {
  if (is.null(wind_speed) || length(wind_speed) == 0) {
    return(NA_real_)
  }
  nums <- suppressWarnings(as.numeric(unlist(regmatches(
    as.character(wind_speed),
    gregexpr("[0-9]+", as.character(wind_speed))
  ))))
  if (length(nums) == 0 || all(is.na(nums))) {
    return(NA_real_)
  }
  max(nums, na.rm = TRUE)
}

wmw_add_task <- function(tasks, title, detail) {
  tasks[[length(tasks) + 1]] <- list(title = title, detail = detail)
  tasks
}

wmw_clean_day_label <- function(name) {
  nm <- as.character(name)
  nm <- sub(" Night$", "", nm, ignore.case = TRUE)
  nm
}

#' Daytime strip summary for the rolling forecast (one row per daytime period).
wmw_forecast_day_strip <- function(forecast) {
  if (is.null(forecast) || nrow(forecast) == 0) {
    return(data.frame(
      day = character(),
      temp = numeric(),
      precip = numeric(),
      wind = numeric(),
      short = character(),
      stringsAsFactors = FALSE
    ))
  }

  day <- forecast[forecast$is_daytime %in% TRUE, , drop = FALSE]
  if (nrow(day) == 0) {
    day <- forecast
  }

  data.frame(
    day = vapply(day$name, wmw_clean_day_label, character(1)),
    temp = as.numeric(day$temperature),
    precip = as.numeric(day$probability_of_precipitation),
    wind = vapply(day$wind_speed, wmw_parse_wind_mph, numeric(1)),
    short = as.character(day$short_forecast),
    stringsAsFactors = FALSE
  )
}

#' Score the week: warmest / wettest / windiest days + simple pattern.
wmw_week_weather_facts <- function(forecast) {
  strip <- wmw_forecast_day_strip(forecast)
  empty <- list(
    strip = strip,
    warmest = NULL,
    wettest = NULL,
    windiest = NULL,
    dry_days = character(),
    pattern = "unknown",
    max_high = NA_real_,
    max_precip = NA_real_,
    max_wind = NA_real_
  )
  if (nrow(strip) == 0) {
    return(empty)
  }

  strip$precip[!is.finite(strip$precip)] <- 0
  strip$wind[!is.finite(strip$wind)] <- 0

  i_warm <- which.max(strip$temp)
  i_wet <- which.max(strip$precip)
  i_wind <- which.max(strip$wind)

  dry_days <- strip$day[strip$precip < 30 & is.finite(strip$temp)]
  n <- nrow(strip)
  early <- strip[seq_len(min(3L, n)), , drop = FALSE]
  late <- strip[seq.int(max(1L, n - 2L), n), , drop = FALSE]
  early_wet <- mean(early$precip, na.rm = TRUE)
  late_wet <- mean(late$precip, na.rm = TRUE)

  pattern <- if (isTRUE(early_wet < 28) && (isTRUE(late_wet >= 35) || isTRUE(max(late$precip, na.rm = TRUE) >= 55))) {
    "nice_then_wet"
  } else if (isTRUE(early_wet >= 40) && isTRUE(late_wet < 28)) {
    "wet_then_nice"
  } else if (isTRUE(max(strip$precip, na.rm = TRUE) >= 55)) {
    "stormy_lurking"
  } else if (isTRUE(max(strip$wind, na.rm = TRUE) >= 18) && isTRUE(max(strip$precip, na.rm = TRUE) < 40)) {
    "windy_dry"
  } else if (isTRUE(max(strip$precip, na.rm = TRUE) < 30)) {
    "mostly_fair"
  } else {
    "mixed"
  }

  list(
    strip = strip,
    warmest = strip[i_warm, , drop = FALSE],
    wettest = strip[i_wet, , drop = FALSE],
    windiest = strip[i_wind, , drop = FALSE],
    dry_days = dry_days,
    pattern = pattern,
    max_high = max(strip$temp, na.rm = TRUE),
    max_precip = max(strip$precip, na.rm = TRUE),
    max_wind = max(strip$wind, na.rm = TRUE)
  )
}

wmw_fmt_temp <- function(x) {
  if (!is.finite(x)) return("—")
  paste0(round(x, 0), "°F")
}

wmw_fmt_precip <- function(x) {
  if (!is.finite(x)) return("—")
  paste0(round(x, 0), "%")
}

wmw_fmt_wind <- function(x) {
  if (!is.finite(x)) return("—")
  paste0(round(x, 0), " mph")
}

#' Weekly Wooden Man tasks — day-aware from the 7-day strip + season.
#'
#' Monday editors: prefer `wmw_manual_monday_tasks` at the top of this file.
#' Auto rules below only need edits when you change the logic itself.
wmw_monday_tasks <- function(
  work_week = NULL,
  current = NULL,
  rolling = NULL,
  today = lubridate::with_tz(Sys.time(), "America/Detroit")
) {
  tasks <- list()
  month <- lubridate::month(today)
  facts <- wmw_week_weather_facts(work_week)

  # Manual Monday tasks first
  if (length(wmw_manual_monday_tasks) > 0) {
    for (t in wmw_manual_monday_tasks) {
      if (!is.null(t$title) && !is.null(t$detail)) {
        tasks <- wmw_add_task(tasks, t$title, t$detail)
      }
    }
  }

  # --- Week pattern (specific to this forecast) --------------------------------
  if (identical(facts$pattern, "nice_then_wet") && !is.null(facts$warmest)) {
    w <- facts$warmest
    wet <- facts$wettest
    tasks <- wmw_add_task(
      tasks,
      paste0("Use ", w$day, " while it is still easy"),
      paste0(
        "This week starts kinder than it finishes. ",
        w$day, " looks like the outdoor gift (near ", wmw_fmt_temp(w$temp), ", ",
        tolower(w$short), "). Hammock, Fit Strip, or Presque Isle rocks — stay out longer than you meant to. ",
        "Save flexibility for ", wet$day, " when precip odds climb (", wmw_fmt_precip(wet$precip), ")."
      )
    )
  } else if (identical(facts$pattern, "wet_then_nice") && !is.null(facts$warmest)) {
    w <- facts$warmest
    wet <- facts$wettest
    tasks <- wmw_add_task(
      tasks,
      paste0("Hold for ", w$day, "; soft start if it is wet"),
      paste0(
        "Early days look wetter (", wet$day, " ~", wmw_fmt_precip(wet$precip),
        "). Peter White Library, Dead River Coffee, or MarqTran to Ishpeming if you need a stretch indoors. ",
        "Then spend ", w$day, " outside near ", wmw_fmt_temp(w$temp), " — shore sit or Fit Strip when it clears."
      )
    )
  } else if (identical(facts$pattern, "mostly_fair") && !is.null(facts$warmest)) {
    w <- facts$warmest
    tasks <- wmw_add_task(
      tasks,
      "Fair stretch — bank outdoor time",
      paste0(
        "Precip stays mostly quiet this week. Peak mild looks like ", w$day,
        " near ", wmw_fmt_temp(w$temp), ". Picnic blanket, knitting outside, or a slow Presque Isle wander. ",
        "No gear needed for a shoreline sit."
      )
    )
  } else if (identical(facts$pattern, "windy_dry") && !is.null(facts$windiest)) {
    wi <- facts$windiest
    tasks <- wmw_add_task(
      tasks,
      paste0(wi$day, " is the wind day"),
      paste0(
        "Dry enough to be out, breezy enough to feel it — up toward ", wmw_fmt_wind(wi$wind),
        " on ", wi$day, ". Kite at Presque Isle if you have one; otherwise lean into the lake wind on a shore-path bench. ",
        "Board folks: watch the lower harbor / Ore Dock even if you stay ashore."
      )
    )
  } else if (identical(facts$pattern, "stormy_lurking") && !is.null(facts$wettest)) {
    wet <- facts$wettest
    tasks <- wmw_add_task(
      tasks,
      paste0(wet$day, " needs a soft plan"),
      paste0(
        wet$day, " carries the wettest odds (~", wmw_fmt_precip(wet$precip), ", ",
        tolower(wet$short), "). Library (217 N Front — closed Sundays), Dead River on Baraga, ",
        "or try a downtown place you have not done this season. If blue breaks open, steal a short Fit Strip loop."
      )
    )
  } else if (!is.null(facts$warmest)) {
    w <- facts$warmest
    wet <- facts$wettest
    tasks <- wmw_add_task(
      tasks,
      "Mixed week — pick your windows",
      paste0(
        "Warmest look: ", w$day, " near ", wmw_fmt_temp(w$temp), ". ",
        "Wettest look: ", wet$day, " (~", wmw_fmt_precip(wet$precip), "). ",
        "Outdoor on the kinder days (Presque Isle / Fit Strip / hammock); indoor stretch when it turns."
      )
    )
  }

  # --- Named warmest day (if not already the pattern lead and it is worth it) ----
  if (!is.null(facts$warmest) && isTRUE(facts$max_high >= 70) &&
      !identical(facts$pattern, "nice_then_wet") &&
      !identical(facts$pattern, "mostly_fair")) {
    w <- facts$warmest
    tasks <- wmw_add_task(
      tasks,
      paste0(w$day, " warmth"),
      paste0(
        "Week’s mild peak near ", wmw_fmt_temp(w$temp), " on ", w$day,
        ". That is the hammock / Tourist Park / Presque Isle sit day. Cooler evenings still count with a blanket and a friend."
      )
    )
  }

  # --- Named wind day when breezy and not already covered ----------------------
  if (!is.null(facts$windiest) && isTRUE(facts$max_wind >= 15) &&
      !identical(facts$pattern, "windy_dry")) {
    wi <- facts$windiest
    if (isTRUE(wi$precip < 45)) {
      tasks <- wmw_add_task(
        tasks,
        paste0("Catch the breeze on ", wi$day),
        paste0(
          "Winds toward ", wmw_fmt_wind(wi$wind), " on ", wi$day,
          ". Kite or lake-watch; no kite still works on a Presque Isle bench facing Superior."
        )
      )
    }
  }

  # --- Wet day indoor when stormy but pattern was not stormy_lurking -----------
  if (!is.null(facts$wettest) && isTRUE(facts$max_precip >= 50) &&
      !identical(facts$pattern, "stormy_lurking") &&
      !identical(facts$pattern, "nice_then_wet") &&
      !identical(facts$pattern, "wet_then_nice")) {
    wet <- facts$wettest
    tasks <- wmw_add_task(
      tasks,
      paste0("Flex day: ", wet$day),
      paste0(
        "Precip odds near ", wmw_fmt_precip(wet$precip), " — ", tolower(wet$short),
        ". Default indoor: Peter White Library or Dead River Coffee. MarqTran to Ishpeming if you want a small adventure."
      )
    )
  }

  # --- Buck up for approaching winter (late summer through deep fall) ----------
  if (month %in% c(8, 9, 10, 11)) {
    tasks <- wmw_add_task(
      tasks,
      "Buck up for winter",
      paste0(
        "Superior season is turning — get ready before the first lasting lake-effect cycle owns the calendar. ",
        "Stage scrapers and a warm layer by the door; check heat (furnace, baseboard, or wood); ",
        "cabin folks: woodpile and roof edges; renters: same idea in apartment scale. ",
        "Enjoy the mild days, but do not pretend January is optional."
      )
    )
  } else if (month %in% c(12, 1, 2)) {
    tasks <- wmw_add_task(
      tasks,
      "Stay winter-ready",
      "Deep season: keep traction gear where you can reach it, watch freeze-thaw sidewalks, and thaw at Dead River after a short Presque Isle or Fit Strip blast."
    )
  }

  # --- Light seasonal garnish (no grass; weather still leads) ------------------
  if (month %in% c(4, 5) && isTRUE(facts$max_high >= 50)) {
    tasks <- wmw_add_task(
      tasks,
      "Shoulder-season beds",
      "If soil is workable and hard frost is not in the near term, cool-season starts can go in. No plot? Long Fit Strip walks between showers still count."
    )
  }

  if (month %in% c(6, 7) && identical(facts$pattern, "mostly_fair")) {
    tasks <- wmw_add_task(
      tasks,
      "High-summer hang",
      "Long light: shoreline evenings, late hammocks, water-watching from the lower harbor. Apartment version: stoop herbs and a dusk Fit Strip loop."
    )
  }

  if (!is.null(rolling) && nrow(rolling) > 0) {
    latest <- rolling[which.max(rolling$year * 100 + rolling$month), ]
    if (!is.na(latest$snow_obs) && !is.na(latest$snow_normal) &&
        latest$snow_obs > latest$snow_normal * 1.2) {
      tasks <- wmw_add_task(
        tasks,
        "Snow running high vs normal",
        "Recent snowfall is above the 1991–2020 normal. After a dump: short bright walk when it clears, then warm up. Keep the scraper honest."
      )
    }
  }

  # Deduplicate titles
  if (length(tasks) > 1) {
    titles <- vapply(tasks, function(x) x$title, character(1))
    tasks <- tasks[!duplicated(titles)]
  }

  # Cap auto length but never drop manual block
  n_manual <- length(wmw_manual_monday_tasks)
  max_total <- n_manual + 5L
  if (length(tasks) > max_total) {
    tasks <- tasks[seq_len(max_total)]
  }

  if (length(tasks) == 0) {
    tasks <- wmw_add_task(
      tasks,
      "Steady week outside",
      "No loud triggers — still go out: Presque Isle rocks, Fit Strip loop, or hammock with a blanket. Foul backup: Peter White Library, Dead River Coffee, or MarqTran to Ishpeming."
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
