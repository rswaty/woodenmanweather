# Wooden Man Weather — Crisp Precision Weather Instrument Charts
# - Pinned, always-visible Y-axis (fixed on the left, never scrolls off)
# - Divergence gradient on observed line and light shading between curves
# - Crisp white historical line with slightly wider stroke
# - Clean, unobstructed title at top
# - Fluid undulating monthly curves across all three charts
# - 12-Month cumulative totals in the bottom strip for precip & snow
# - Today's high in the bottom strip for temperature
# - Nirmala font styling across all labels
# - 0 decimals for temp and snowfall, 1 decimal for precipitation

source("R/constants.R")

wmw_interactive_chart <- function(
  series,
  metric = c("temperature", "precipitation", "snowfall"),
  chart_id = NULL,
  today_high = NULL,
  today_low = NULL,
  month_total = NULL
) {
  metric <- match.arg(metric)
  if (is.null(chart_id)) {
    chart_id <- paste0("wmw-chart-", metric)
  }

  # Chronological order
  series <- series[order(series$year, series$month), ]
  n_pts <- nrow(series)
  if (n_pts == 0) {
    return(htmltools::HTML(paste0("<div class='wmw-chart-error'>No data available for ", metric, "</div>")))
  }

  secondary_badge_html <- ""
  chart_subtitle <- ""
  max_vals <- NULL
  low_obs_vals <- NULL
  low_norm_vals <- NULL

  # Metric specific divergence palettes & calculations
  if (metric == "temperature") {
    obs_col <- "tmax_obs"
    norm_col <- "tmax_normal"
    unit <- "°F"
    tick_unit <- "°"
    unit_badge <- "°F"
    digits <- 0
    title <- "Monthly Avg Temps vs. Recent Historical Norms"
    chart_subtitle <- paste0(
      "Dashed lines = historical · Bold = avg highs · Lighter = avg lows",
      "<span class='wmw-chart-subtitle-line'>Diamonds = hottest day of each month this year</span>"
    )
    pos_color <- "#f59e0b"        # warm amber (above normal)
    neg_color <- "#38bdf8"        # crisp slate-cyan (below normal)

    obs_vals <- as.numeric(series[[obs_col]])
    norm_vals <- as.numeric(series[[norm_col]])
    low_obs_vals <- if ("tmin_obs" %in% names(series)) as.numeric(series$tmin_obs) else rep(NA_real_, n_pts)
    low_norm_vals <- if ("tmin_normal" %in% names(series)) as.numeric(series$tmin_normal) else rep(NA_real_, n_pts)
    max_vals <- if ("tmax_max" %in% names(series)) as.numeric(series$tmax_max) else obs_vals

    # Today's high/low in the bubble + rightmost mean points
    latest_norm <- norm_vals[n_pts]
    latest_obs <- if (!is.null(today_high) && !is.na(today_high)) as.numeric(today_high) else obs_vals[n_pts]
    obs_vals[n_pts] <- latest_obs

    if (!is.null(today_low) && !is.na(today_low)) {
      low_obs_vals[n_pts] <- as.numeric(today_low)
    }

    # Current-month hottest day includes today's forecast/obs high when warmer
    if (!is.na(max_vals[n_pts])) {
      max_vals[n_pts] <- max(max_vals[n_pts], latest_obs, na.rm = TRUE)
    } else {
      max_vals[n_pts] <- latest_obs
    }

    latest_diff <- latest_obs - latest_norm
    diff_rounded <- round(latest_diff, 0)
    if (abs(diff_rounded) == 0) diff_rounded <- 0
    diff_sign <- if (diff_rounded > 0) "+" else ""
    diff_str <- paste0(diff_sign, diff_rounded, " °F")
    latest_obs_str <- paste0(round(latest_obs, 0), " °F")

    stat_label <- "Today's High:"
    diff_context <- "vs. recent normal"
    curr_color <- if (latest_obs >= latest_norm) pos_color else neg_color

    if (!is.null(today_low) && !is.na(today_low)) {
      low_str <- paste0(round(as.numeric(today_low), 0), " °F")
      secondary_badge_html <- paste0(
        "<div class='wmw-stat-badge'>",
        "<span class='wmw-stat-now-label'>Today's Low:</span>",
        "<span class='wmw-stat-now-value'>", low_str, "</span>",
        "</div>"
      )
    }

  } else if (metric == "precipitation") {
    unit <- "in"
    tick_unit <- " in"
    unit_badge <- "in"
    digits <- 1                    # exactly one tenth
    title <- "Precipitation vs. Recent Historical Norms"
    pos_color <- "#10b981"        # crisp emerald (wetter than normal)
    neg_color <- "#f59e0b"        # warm golden-amber (drier than normal)

    # Monthly amounts for wave-like visual curve
    obs_vals <- as.numeric(series$prcp_obs)
    norm_vals <- as.numeric(series$prcp_normal)

    # Cumulative 12-month totals for bottom summary bubble
    cum_obs <- sum(obs_vals, na.rm = TRUE)
    cum_norm <- sum(norm_vals, na.rm = TRUE)
    cum_diff <- cum_obs - cum_norm

    diff_rounded <- round(cum_diff, digits)
    if (abs(diff_rounded) == 0) diff_rounded <- 0
    diff_sign <- if (diff_rounded > 0) "+" else ""
    diff_str <- paste0(diff_sign, format(diff_rounded, nsmall = digits), " in")
    latest_obs_str <- paste0(format(round(cum_obs, digits), nsmall = digits), " in")

    stat_label <- "12-Mo Total:"
    diff_context <- "vs. recent normal"
    curr_color <- if (cum_diff >= 0) pos_color else neg_color

    if (!is.null(month_total) && !is.na(month_total)) {
      mo_str <- paste0(format(round(as.numeric(month_total), digits), nsmall = digits), " in")
      secondary_badge_html <- paste0(
        "<div class='wmw-stat-badge'>",
        "<span class='wmw-stat-now-label'>1-Mo Total:</span>",
        "<span class='wmw-stat-now-value'>", mo_str, "</span>",
        "<span class='wmw-stat-now-diff' style='color: #94a3b8;'>(last 30 days)</span>",
        "</div>"
      )
    }

  } else { # snowfall
    unit <- "in"
    tick_unit <- " in"
    unit_badge <- "in"
    digits <- 0
    title <- "Snowfall vs. Recent Historical Norms"
    pos_color <- "#a78bfa"        # soft violet lavender (above normal)
    neg_color <- "#818cf8"        # soft periwinkle slate (below normal)

    # Monthly amounts for wave-like visual curve
    obs_vals <- as.numeric(series$snow_obs)
    norm_vals <- as.numeric(series$snow_normal)

    # Cumulative 12-month totals for bottom summary bubble
    cum_obs <- sum(obs_vals, na.rm = TRUE)
    cum_norm <- sum(norm_vals, na.rm = TRUE)
    cum_diff <- cum_obs - cum_norm

    diff_rounded <- round(cum_diff, digits)
    if (abs(diff_rounded) == 0) diff_rounded <- 0
    diff_sign <- if (diff_rounded > 0) "+" else ""
    diff_str <- paste0(diff_sign, format(diff_rounded, nsmall = digits), " in")
    latest_obs_str <- paste0(format(round(cum_obs, digits), nsmall = digits), " in")

    stat_label <- "12-Mo Total:"
    diff_context <- "vs. recent normal"
    curr_color <- if (cum_diff >= 0) pos_color else neg_color

    if (!is.null(month_total) && !is.na(month_total)) {
      mo_str <- paste0(round(as.numeric(month_total), digits), " in")
      secondary_badge_html <- paste0(
        "<div class='wmw-stat-badge'>",
        "<span class='wmw-stat-now-label'>1-Mo Total:</span>",
        "<span class='wmw-stat-now-value'>", mo_str, "</span>",
        "<span class='wmw-stat-now-diff' style='color: #94a3b8;'>(last 30 days)</span>",
        "</div>"
      )
    }
  }

  # Canvas coordinate space:
  # Fixed Y-Axis SVG: 58 x 230
  # Timeline SVG: 900 x 230
  view_h <- 230
  pad_top <- 20
  pad_bottom <- 36
  plot_h <- view_h - pad_top - pad_bottom

  # Y range calculation — include highs, lows, and hottest-day markers
  all_vals <- c(obs_vals, norm_vals, max_vals, low_obs_vals, low_norm_vals)
  all_vals <- all_vals[!is.na(all_vals)]
  val_min <- min(all_vals)
  val_max <- max(all_vals)
  span <- val_max - val_min
  if (span == 0) span <- 1

  if (metric %in% c("precipitation", "snowfall")) {
    y_min <- 0
    y_max <- val_max * 1.15
  } else {
    y_min <- val_min - (span * 0.12)
    y_max <- val_max + (span * 0.16)
  }

  # Y scale function (identical in both SVGs)
  y_scale <- function(v) {
    pad_top + (1 - (v - y_min) / (y_max - y_min)) * plot_h
  }

  y_obs <- vapply(obs_vals, y_scale, numeric(1))
  y_norm <- vapply(norm_vals, y_scale, numeric(1))
  y_low_obs <- if (!is.null(low_obs_vals)) vapply(low_obs_vals, y_scale, numeric(1)) else NULL
  y_low_norm <- if (!is.null(low_norm_vals)) vapply(low_norm_vals, y_scale, numeric(1)) else NULL

  baseline_y <- view_h - pad_bottom + 4

  # HTML Y-axis (same font path as badges — never SVG-squashed)
  raw_ticks <- pretty(c(if (metric %in% c("precipitation", "snowfall")) 0 else val_min, val_max), n = 4)
  valid_ticks <- raw_ticks[raw_ticks >= y_min & raw_ticks <= y_max]
  if (length(valid_ticks) == 0) valid_ticks <- raw_ticks

  y_labels_html <- character(length(valid_ticks))
  for (j in seq_along(valid_ticks)) {
    tv <- valid_ticks[j]
    ty <- y_scale(tv)
    top_pct <- round((ty / view_h) * 100, 2)
    lbl_val <- if (digits == 0) round(tv, 0) else format(round(tv, digits), nsmall = digits)
    tick_lbl <- paste0(lbl_val, tick_unit)
    y_labels_html[j] <- paste0(
      "<div class='wmw-y-label' style='top:", top_pct, "%;'>", tick_lbl, "</div>"
    )
  }

  fixed_y_axis_html <- paste0(
"<div class='wmw-y-axis-track'>
  <div class='wmw-y-unit'>", unit_badge, "</div>
  <div class='wmw-y-spine'></div>
  ", paste(y_labels_html, collapse = "\n  "), "
</div>")

  # Build Scrollable Timeline SVG
  view_w_timeline <- 900
  pad_left_time <- 20
  pad_right_time <- 35
  plot_w_time <- view_w_timeline - pad_left_time - pad_right_time

  # X coordinates
  x_step <- if (n_pts > 1) plot_w_time / (n_pts - 1) else plot_w_time
  x_coords <- pad_left_time + (seq_len(n_pts) - 1) * x_step

  # Build smooth cubic Bezier path string
  build_spline_path <- function(xs, ys) {
    n <- length(xs)
    if (n == 1) return(paste0("M ", xs[1], " ", ys[1]))
    path <- paste0("M ", round(xs[1], 1), " ", round(ys[1], 1))
    for (i in 1:(n - 1)) {
      x0 <- if (i == 1) xs[1] else xs[i - 1]
      y0 <- if (i == 1) ys[1] else ys[i - 1]
      x1 <- xs[i]
      y1 <- ys[i]
      x2 <- xs[i + 1]
      y2 <- ys[i + 1]
      x3 <- if (i + 2 <= n) xs[i + 2] else xs[n]
      y3 <- if (i + 2 <= n) ys[i + 2] else ys[n]

      cp1x <- x1 + (x2 - x0) / 6
      cp1y <- y1 + (y2 - y0) / 6
      cp2x <- x2 - (x3 - x1) / 6
      cp2y <- y2 - (y3 - y1) / 6

      path <- paste0(
        path, " C ",
        round(cp1x, 1), " ", round(cp1y, 1), ", ",
        round(cp2x, 1), " ", round(cp2y, 1), ", ",
        round(x2, 1), " ", round(y2, 1)
      )
    }
    path
  }

  obs_path <- build_spline_path(x_coords, y_obs)
  norm_path <- build_spline_path(x_coords, y_norm)
  low_obs_path <- if (!is.null(y_low_obs)) build_spline_path(x_coords, y_low_obs) else NULL
  low_norm_path <- if (!is.null(y_low_norm)) build_spline_path(x_coords, y_low_norm) else NULL

  # Divergence area polygon enclosed strictly between observed and historical curves
  rev_norm_path_pts <- paste0(" L ", rev(round(x_coords, 1)), " ", rev(round(y_norm, 1)), collapse = "")
  divergence_area_path <- paste0(obs_path, rev_norm_path_pts, " Z")

  # Divergence gradient stops for line and enclosed shading
  grad_stops_line <- character(n_pts)
  grad_stops_area <- character(n_pts)

  for (i in seq_len(n_pts)) {
    pct <- round(((x_coords[i] - pad_left_time) / plot_w_time) * 100, 1)
    is_pos <- obs_vals[i] >= norm_vals[i]
    c <- if (is_pos) pos_color else neg_color
    grad_stops_line[i] <- paste0("<stop offset='", pct, "%' stop-color='", c, "' />")
    grad_stops_area[i] <- paste0("<stop offset='", pct, "%' stop-color='", c, "' stop-opacity='0.25' />")
  }
  grad_line_str <- paste(grad_stops_line, collapse = "\n")
  grad_area_str <- paste(grad_stops_area, collapse = "\n")

  # Lighter divergence stroke for observed lows
  low_grad_stops_line <- character(n_pts)
  if (!is.null(low_obs_vals) && !is.null(low_norm_vals)) {
    for (i in seq_len(n_pts)) {
      pct <- round(((x_coords[i] - pad_left_time) / plot_w_time) * 100, 1)
      is_pos <- !is.na(low_obs_vals[i]) && !is.na(low_norm_vals[i]) && low_obs_vals[i] >= low_norm_vals[i]
      c <- if (is_pos) pos_color else neg_color
      low_grad_stops_line[i] <- paste0("<stop offset='", pct, "%' stop-color='", c, "' />")
    }
  }
  low_grad_line_str <- paste(low_grad_stops_line, collapse = "\n")

  grad_line_id <- paste0("grad-line-", metric)
  grad_area_id <- paste0("grad-area-", metric)
  low_grad_line_id <- paste0("grad-low-line-", metric)

  # HTML X-axis labels (badge fonts) + SVG tick marks only
  month_ticks_svg <- character(n_pts)
  x_labels_html <- character(n_pts)
  for (i in seq_len(n_pts)) {
    yr_short <- substr(as.character(series$year[i]), 3, 4)
    lbl <- if (i == 1 || i == n_pts || series$month[i] == 1) {
      paste0(series$month_label[i], " '", yr_short)
    } else {
      series$month_label[i]
    }
    is_latest <- (i == n_pts)
    cls <- if (is_latest) "wmw-x-label active" else "wmw-x-label"
    left_pct <- round((x_coords[i] / view_w_timeline) * 100, 2)
    month_ticks_svg[i] <- paste0(
      "<line x1='", round(x_coords[i], 1), "' y1='", baseline_y, "' x2='", round(x_coords[i], 1), "' y2='", baseline_y + 5, "' stroke='#334155' stroke-width='1.5' />"
    )
    x_labels_html[i] <- paste0(
      "<span class='", cls, "' style='left:", left_pct, "%;'>", lbl, "</span>"
    )
  }
  month_ticks_svg_str <- paste(month_ticks_svg, collapse = "\n")
  x_labels_html_str <- paste(x_labels_html, collapse = "\n")

  curr_x <- round(x_coords[n_pts], 1)
  curr_y <- round(y_obs[n_pts], 1)
  curr_dot_color <- if (obs_vals[n_pts] >= norm_vals[n_pts]) pos_color else neg_color
  curr_low_y <- if (!is.null(y_low_obs) && !is.na(y_low_obs[n_pts])) round(y_low_obs[n_pts], 1) else NULL
  curr_low_color <- if (!is.null(low_obs_vals) && !is.null(low_norm_vals) &&
                        !is.na(low_obs_vals[n_pts]) && !is.na(low_norm_vals[n_pts]) &&
                        low_obs_vals[n_pts] >= low_norm_vals[n_pts]) pos_color else neg_color

  low_lines_svg <- ""
  if (!is.null(low_norm_path) && !is.null(low_obs_path)) {
    low_lines_svg <- paste0(
"<!-- Historical lows: lighter dashed white -->
<path d='", low_norm_path, "' stroke='#ffffff' stroke-width='1.6' stroke-dasharray='5 5' opacity='0.42' fill='none' class='wmw-low-normal-line' />

<!-- Observed lows: lighter divergence stroke -->
<path d='", low_obs_path, "' stroke='url(#", low_grad_line_id, ")' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round' opacity='0.55' fill='none' class='wmw-low-obs-line' />
"
    )
  }

  low_grad_def <- if (nzchar(low_grad_line_str)) {
    paste0(
"<linearGradient id='", low_grad_line_id, "' x1='0%' y1='0%' x2='100%' y2='0%'>
", low_grad_line_str, "
</linearGradient>
"
    )
  } else {
    ""
  }

  # Build timeline SVG content — fill the box the same in card and expand
  timeline_svg_html <- paste0(
"<svg class='wmw-svg' viewBox='0 0 ", view_w_timeline, " ", view_h, "' preserveAspectRatio='none'>
<defs>
<linearGradient id='", grad_line_id, "' x1='0%' y1='0%' x2='100%' y2='0%'>
", grad_line_str, "
</linearGradient>
<linearGradient id='", grad_area_id, "' x1='0%' y1='0%' x2='100%' y2='0%'>
", grad_area_str, "
</linearGradient>
", low_grad_def, "</defs>
<rect x='0' y='0' width='", view_w_timeline, "' height='", view_h, "' class='wmw-bg-rect' />

<!-- Light shading strictly between current observed and historical high lines -->
<path d='", divergence_area_path, "' fill='url(#", grad_area_id, ")' class='wmw-area-divergence' />

", low_lines_svg, "
<!-- Historical highs: crisp white and slightly wider (stroke-width 2.6) -->
<path d='", norm_path, "' stroke='#ffffff' stroke-width='2.6' stroke-dasharray='6 4' opacity='0.95' fill='none' class='wmw-normal-line' />

<!-- Observed monthly-average highs -->
<path d='", obs_path, "' stroke='url(#", grad_line_id, ")' stroke-width='2.8' stroke-linecap='round' stroke-linejoin='round' fill='none' class='wmw-obs-line' />

<!-- X-Axis baseline and month tick marks -->
<line x1='0' y1='", baseline_y, "' x2='", view_w_timeline, "' y2='", baseline_y, "' stroke='#334155' stroke-width='1.5' />
", month_ticks_svg_str, "

<!-- Mean-high markers -->
<g class='wmw-markers-group'>")

  for (i in seq_len(n_pts)) {
    is_latest <- (i == n_pts)
    pt_color <- if (obs_vals[i] >= norm_vals[i]) pos_color else neg_color
    if (!is_latest) {
      timeline_svg_html <- paste0(timeline_svg_html, "
<circle cx='", round(x_coords[i], 1), "' cy='", round(y_obs[i], 1), "' r='5.5' fill='", pt_color, "' stroke='#090d13' stroke-width='1.4' class='wmw-dot' />")
    }
    if (!is.null(y_low_obs) && !is_latest && !is.na(y_low_obs[i])) {
      low_pt <- if (!is.null(low_obs_vals) && !is.null(low_norm_vals) &&
                    !is.na(low_obs_vals[i]) && !is.na(low_norm_vals[i]) &&
                    low_obs_vals[i] >= low_norm_vals[i]) pos_color else neg_color
      timeline_svg_html <- paste0(timeline_svg_html, "
<circle cx='", round(x_coords[i], 1), "' cy='", round(y_low_obs[i], 1), "' r='4.2' fill='", low_pt, "' fill-opacity='0.55' stroke='#090d13' stroke-width='1.2' class='wmw-low-dot' />")
    }
  }

  # Monthly hottest-day diamonds (temperature only)
  if (!is.null(max_vals)) {
    timeline_svg_html <- paste0(timeline_svg_html, "
</g>
<g class='wmw-max-group'>")
    for (i in seq_len(n_pts)) {
      if (is.na(max_vals[i]) || is.na(obs_vals[i])) next
      if (max_vals[i] <= obs_vals[i] + 0.5) next
      mx <- round(x_coords[i], 1)
      my_mean <- round(y_obs[i], 1)
      my_max <- round(y_scale(max_vals[i]), 1)
      timeline_svg_html <- paste0(timeline_svg_html, "
<line x1='", mx, "' y1='", my_mean, "' x2='", mx, "' y2='", my_max, "' stroke='#f59e0b' stroke-width='1.2' stroke-opacity='0.45' />
<polygon points='", mx, ",", my_max - 6.5, " ", mx + 6.5, ",", my_max, " ", mx, ",", my_max + 6.5, " ", mx - 6.5, ",", my_max, "' fill='#f59e0b' stroke='#090d13' stroke-width='1.2' class='wmw-max-dot' data-cx='", mx, "' data-cy='", my_max, "' />")
    }
    timeline_svg_html <- paste0(timeline_svg_html, "
</g>
<g class='wmw-markers-group'>")
  }

  low_current_dot <- if (!is.null(curr_low_y)) {
    paste0("
<circle cx='", curr_x, "' cy='", curr_low_y, "' r='5.5' fill='", curr_low_color, "' fill-opacity='0.6' stroke='#ffffff' stroke-width='1.6' class='wmw-low-dot-current' />")
  } else {
    ""
  }

  timeline_svg_html <- paste0(timeline_svg_html, "
<circle cx='", curr_x, "' cy='", curr_y, "' r='7' fill='", curr_dot_color, "' stroke='#ffffff' stroke-width='2' class='wmw-dot-current' />",
low_current_dot, "
</g>
</svg>")

  subtitle_html <- if (nzchar(chart_subtitle)) {
    paste0("<div class='wmw-chart-subtitle'>", chart_subtitle, "</div>")
  } else {
    ""
  }

  html <- paste0(
"<div class='wmw-chart-card' id='", chart_id, "' data-metric='", metric, "'>
<div class='wmw-chart-header'>
<h4 class='wmw-chart-title'>", title, "</h4>
", subtitle_html, "
</div>
<div class='wmw-chart-main'>
<div class='wmw-y-axis-pane'>
", fixed_y_axis_html, "
</div>
<div class='wmw-viewport-shell'>
<div class='wmw-viewport' id='", chart_id, "-viewport' tabindex='0'>
<div class='wmw-canvas-wrap' id='", chart_id, "-canvas' style='width: 135%; min-width: 540px;'>
<div class='wmw-plot-frame'>
", timeline_svg_html, "
</div>
<div class='wmw-x-labels'>
", x_labels_html_str, "
</div>
</div>
</div>
</div>
</div>

<div class='wmw-chart-bottom-bar'>
<div class='wmw-bottom-stats'>
<div class='wmw-stat-badge'>
<span class='wmw-stat-now-label'>", stat_label, "</span>
<span class='wmw-stat-now-value'>", latest_obs_str, "</span>
<span class='wmw-stat-now-diff' style='color: ", curr_color, ";'>(", diff_str, " ", diff_context, ")</span>
</div>
", secondary_badge_html, "
</div>
</div>
<script>
(function() {
  const chartId = '", chart_id, "';
  const viewport = document.getElementById(chartId + '-viewport');
  const canvas = document.getElementById(chartId + '-canvas');

  if (!viewport || !canvas) return;

  let userHasInteracted = false;

  function scrollToEnd() {
    if (userHasInteracted) return;
    requestAnimationFrame(() => {
      viewport.scrollLeft = viewport.scrollWidth - viewport.clientWidth;
    });
  }

  // Counteract preserveAspectRatio=none stretch so markers stay circular
  function fixMarkerAspect() {
    const svg = canvas.querySelector('.wmw-svg');
    if (!svg || !svg.viewBox || !svg.viewBox.baseVal) return;
    const vb = svg.viewBox.baseVal;
    const rect = svg.getBoundingClientRect();
    if (!rect.width || !rect.height || !vb.width || !vb.height) return;
    const sx = rect.width / vb.width;
    const sy = rect.height / vb.height;
    if (!isFinite(sx) || !isFinite(sy) || sx <= 0 || sy <= 0) return;
    const scaleX = sy / sx;

    svg.querySelectorAll('circle.wmw-dot, circle.wmw-low-dot, circle.wmw-dot-current, circle.wmw-low-dot-current').forEach((el) => {
      const cx = el.getAttribute('cx');
      const cy = el.getAttribute('cy');
      if (cx == null || cy == null) return;
      el.setAttribute('transform', `translate(${cx} ${cy}) scale(${scaleX} 1) translate(${-cx} ${-cy})`);
    });

    svg.querySelectorAll('polygon.wmw-max-dot').forEach((el) => {
      const cx = el.getAttribute('data-cx');
      const cy = el.getAttribute('data-cy');
      if (cx == null || cy == null) return;
      el.setAttribute('transform', `translate(${cx} ${cy}) scale(${scaleX} 1) translate(${-cx} ${-cy})`);
    });
  }

  scrollToEnd();
  setTimeout(scrollToEnd, 50);
  setTimeout(scrollToEnd, 200);
  fixMarkerAspect();
  setTimeout(fixMarkerAspect, 50);
  setTimeout(fixMarkerAspect, 250);

  if (window.ResizeObserver) {
    new ResizeObserver(() => {
      if (!userHasInteracted) scrollToEnd();
      fixMarkerAspect();
    }).observe(viewport);
    new ResizeObserver(() => fixMarkerAspect()).observe(canvas);
  }

  viewport.addEventListener('wheel', (e) => {
    userHasInteracted = true;
    if (Math.abs(e.deltaY) > Math.abs(e.deltaX)) {
      e.preventDefault();
      viewport.scrollLeft += e.deltaY;
    }
  }, { passive: false });

  let isDown = false;
  let startX = 0;
  let scrollLeft = 0;

  viewport.addEventListener('mousedown', (e) => {
    isDown = true;
    userHasInteracted = true;
    viewport.classList.add('grabbing');
    startX = e.pageX - viewport.offsetLeft;
    scrollLeft = viewport.scrollLeft;
  });

  window.addEventListener('mouseup', () => {
    isDown = false;
    viewport.classList.remove('grabbing');
  });

  viewport.addEventListener('mousemove', (e) => {
    if (isDown) {
      e.preventDefault();
      const x = e.pageX - viewport.offsetLeft;
      const walk = (x - startX) * 1.5;
      viewport.scrollLeft = scrollLeft - walk;
    }
  });
})();
</script>
</div>")


  htmltools::HTML(html)
}
