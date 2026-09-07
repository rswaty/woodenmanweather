# Optional local cache refresh for climate series.
# GitHub Actions renders with live API calls; run this before a long offline render.

source("R/constants.R")
source("R/climate.R")

end_date <- Sys.Date()
start_date <- end_date - 365

daily <- wmw_ncei_daily(start_date, end_date)
dir.create("data", showWarnings = FALSE)
readr::write_csv(daily, file.path("data", "climate_daily.csv"))

normals <- wmw_ncei_monthly_normals()
readr::write_csv(normals, file.path("data", "climate_normals.csv"))

message("Wrote ", nrow(daily), " daily rows and ", nrow(normals), " normal months.")
