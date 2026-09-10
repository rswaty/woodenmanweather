source("R/constants.R")

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

#' Fair vs foul outdoor week from NWS work-week periods.
wmw_week_outdoor_mood <- function(work_week) {
  if (is.null(work_week) || nrow(work_week) == 0) {
    return(list(
      foul = FALSE,
      breezy = FALSE,
      mild = FALSE,
      precip_chance = NA_real_,
      max_high = NA_real_,
      max_wind = NA_real_
    ))
  }

  precip_chance <- suppressWarnings(max(work_week$probability_of_precipitation, na.rm = TRUE))
  if (!is.finite(precip_chance)) precip_chance <- NA_real_

  daytime <- work_week[work_week$is_daytime %in% TRUE, , drop = FALSE]
  max_high <- if (nrow(daytime) > 0) {
    suppressWarnings(max(daytime$temperature, na.rm = TRUE))
  } else {
    suppressWarnings(max(work_week$temperature, na.rm = TRUE))
  }
  if (!is.finite(max_high)) max_high <- NA_real_

  winds <- vapply(work_week$wind_speed, wmw_parse_wind_mph, numeric(1))
  max_wind <- suppressWarnings(max(winds, na.rm = TRUE))
  if (!is.finite(max_wind)) max_wind <- NA_real_

  forecast_blob <- tolower(paste(
    work_week$short_forecast,
    work_week$detailed_forecast,
    collapse = " "
  ))
  severe_words <- grepl(
    "heavy rain|flood|blizzard|ice storm|freezing rain|wintry mix|lake effect snow|snow shower|severe thunderstorm",
    forecast_blob
  )
  chance_storms <- grepl("thunder", forecast_blob)

  # Foul = clearly wet/cold or severe — not a lone "chance thunderstorms" on a warm day
  foul <- isTRUE(precip_chance >= 60) ||
    (isTRUE(precip_chance >= 45) && isTRUE(max_high < 55)) ||
    severe_words ||
    (chance_storms && isTRUE(precip_chance >= 50) && isTRUE(max_high < 65))

  breezy <- isTRUE(max_wind >= 15)
  mild <- isTRUE(max_high >= 60)

  list(
    foul = foul,
    breezy = breezy,
    mild = mild,
    precip_chance = precip_chance,
    max_high = max_high,
    max_wind = max_wind
  )
}

wmw_add_task <- function(tasks, title, detail) {
  tasks[[length(tasks) + 1]] <- list(title = title, detail = detail)
  tasks
}

#' Weekly Wooden Man tasks — outdoor Marquette geography first; foul-weather indoor stretch.
wmw_monday_tasks <- function(
  work_week = NULL,
  current = NULL,
  rolling = NULL,
  today = lubridate::with_tz(Sys.time(), "America/Detroit")
) {
  tasks <- list()

  month <- lubridate::month(today)
  weekday <- lubridate::wday(today, week_start = 1)
  mood <- wmw_week_outdoor_mood(work_week)

  if (weekday != 1) {
    tasks <- wmw_add_task(
      tasks,
      "Briefing cadence",
      "This weekly briefing refreshes for Monday mornings. The outdoor ideas still work any day — check back at the start of the week for a fresh set."
    )
  }

  # --- Core week: fair outdoors vs foul indoors ---------------------------------
  if (isTRUE(mood$foul)) {
    tasks <- wmw_add_task(
      tasks,
      "Foul-weather stretch",
      "When Presque Isle is a wash, stretch the day indoors: Peter White Public Library (217 N Front — closed Sundays), a long sit at Dead River Coffee on Baraga, or try a downtown place you have not done this season (Delft, Lagniappe, Vierling, Zephyr)."
    )
    tasks <- wmw_add_task(
      tasks,
      "MarqTran adventure",
      "Ride MarqTran out to Ishpeming or Negaunee, poke around Main Street, warm up somewhere, ride home. Check today’s timetable at marq-tran.com — low-cost foul-day wandering without needing a plan."
    )
    tasks <- wmw_add_task(
      tasks,
      "Breaks of blue",
      "If the sky opens even half an hour, take it: Fit Strip out-and-back, a quick sit on the Presque Isle rocks, or a hammock/blanket with a friend before it closes back in."
    )
  } else {
    tasks <- wmw_add_task(
      tasks,
      "Lakeshore stretch",
      "Start soft outside: ten quiet minutes on the Presque Isle rocks, a Fit Strip loop with nowhere to be, or a hammock and blanket with a friend at Tourist Park. Stay out longer than you meant to."
    )

    if (isTRUE(mood$breezy) || month %in% c(4, 5, 9, 10)) {
      tasks <- wmw_add_task(
        tasks,
        "Wind day",
        "If the breeze is up, it is kite weather on the open grass at Presque Isle — or just lean into the lake wind on a shore path bench. No kite? Same wind works. Board people: watch the water near the lower harbor / Ore Dock even if you are not on one."
      )
    } else {
      tasks <- wmw_add_task(
        tasks,
        "Quiet hands outside",
        "Outdoor knitting, a book, or cards at a Presque Isle picnic table or a sheltered yard nook. Mild evening? Stretch it facing the water at McCarty’s Cove or the peninsula tip."
      )
    }

    if (isTRUE(mood$mild) || month %in% c(6, 7, 8, 9)) {
      tasks <- wmw_add_task(
        tasks,
        "Longer outdoor hang",
        "Picnic blanket, throw, thermos — Fit Strip with sit-stops, a slow Presque Isle wander (lighthouse side, then grass), or hammock round two at dusk. Pointless and nice still counts."
      )
    }
  }

  # Midweek culture nod (stable rhythm; listings move)
  tasks <- wmw_add_task(
    tasks,
    "Midweek local night",
    "Marquette’s small-stage / open-mic energy usually shows up midweek. Check the current week at marquettemusicscene.com before you go — or use it as ‘what’s humming’ while you stay in with tea and a window cracked."
  )

  # --- Seasonal color (living outdoors / lake year), inclusive -------------------
  if (month %in% c(4, 5)) {
    tasks <- wmw_add_task(
      tasks,
      "Mud-season garden beds",
      "If your soil (yard, plot, or a borrowed corner) is workable and hard frost is not looming, cool-season starts like peas and onions can go in. Keep row cover handy. No garden? Same week favors long Fit Strip walks between showers."
    )
  }

  if (month %in% c(6, 7, 8)) {
    tasks <- wmw_add_task(
      tasks,
      "High-summer outside",
      "Warm evenings favor shoreline hangs, late hammocks, and easy water-watching. Hardening off transplants and watering still count as outdoor time — apartment version: herbs on a stoop and a dusk walk on the Fit Strip."
    )
  }

  if (month %in% c(9, 10)) {
    tasks <- wmw_add_task(
      tasks,
      "Before lake-effect settles in",
      "Use the soft fall windows: longer Presque Isle sits, kite if it is breezy, blanket + friend while the grass is still kind. Indoors backup: Peter White Library or Dead River when the first lasting cold rain shows up."
    )
  }

  if (month %in% c(11, 12, 1, 2)) {
    tasks <- wmw_add_task(
      tasks,
      "Cold-season outside, short and honest",
      "Bundle for a brief Presque Isle or Fit Strip blast — face the wind, then thaw at Dead River Coffee. Foul and dark: library chair, leftover soup, window cracked so it still feels like the lake is out there."
    )
  }

  if (month %in% c(3, 4)) {
    tasks <- wmw_add_task(
      tasks,
      "Thaw light",
      "March/April can tease: dry afternoon → shore path or hammock trial run; slick morning → Dead River or Peter White until the peninsula dries. Stretch whatever scrap of blue you get."
    )
  }

  # Climate context without chore framing
  if (!is.null(rolling) && nrow(rolling) > 0) {
    latest <- rolling[which.max(rolling$year * 100 + rolling$month), ]
    if (!is.na(latest$snow_obs) && !is.na(latest$snow_normal) && latest$snow_obs > latest$snow_normal * 1.2) {
      tasks <- wmw_add_task(
        tasks,
        "Snow running high",
        "Recent snowfall is above the 1991–2020 normal. After a dump: short bright walk when it clears, then a warm sit at Dead River or the library. Cabin/roof folks: clear valleys when you can; renters: scraper by the door still counts as readiness."
      )
    }
  }

  # Cap length so the briefing stays scannable
  if (length(tasks) > 6) {
    # Keep cadence note (if any) + prioritize forecast-driven + midweek + one seasonal
    tasks <- tasks[seq_len(6)]
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
