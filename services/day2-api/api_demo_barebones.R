suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# Variable definitions for Day-2 API input:
# age.months: age in months (number of months).
# sex: binary, 1 = male, 0 = female.
# adm.recent: binary, 1 = overnight hospitalisation within last 6 months, 0 = no.
# wfaz: weight-for-age z-score (number).
# cidysymp: duration of illness in days.
# not.alert: AVPU < A, 1 = yes, 0 = no.
# hr.all: heart rate (bpm).
# rr.all: respiratory rate (bpm).
# envhtemp: axillary temperature (degrees C).
# crt.long: capillary refill time > 2 seconds, 1 = yes, 0 = no.
# oxy.ra: oxygen saturation (%).
# LEVEL1_TREATMENTS_D1_SAFE_0 .. LEVEL5_TREATMENTS_D1_SAFE_0:
# binary Day-1 treatment outputs used as Day-2 predictors.

base_url <- Sys.getenv("SEPSIS_FLOW_API_URL", unset = "https://sepsis-flow-platform.onrender.com")

# 1) Health request
health_resp <- httr2::request(paste0(base_url, "/health")) |>
  httr2::req_perform()
cat("Health status:", httr2::resp_status(health_resp), "\n")
cat(httr2::resp_body_string(health_resp), "\n\n")

# 2) Minimal Day-2 prediction request
payload <- list(
  data = list(
    age.months = 24,
    sex = 0,
    adm.recent = 0,
    wfaz = -1.1,
    cidysymp = 2,
    not.alert = 0,
    hr.all = 120,
    rr.all = 28,
    envhtemp = 37.8,
    crt.long = 0,
    oxy.ra = 98,
    LEVEL1_TREATMENTS_D1_SAFE_0 = 0,
    LEVEL2_TREATMENTS_D1_SAFE_0 = 0,
    LEVEL3_TREATMENTS_D1_SAFE_0 = 1,
    LEVEL4_TREATMENTS_D1_SAFE_0 = 0,
    LEVEL5_TREATMENTS_D1_SAFE_0 = 1
  ),
  levels = c("L1", "L3", "L5")
)

predict_resp <- httr2::request(paste0(base_url, "/predict/day2?format=long&vote_threshold=0.5")) |>
  httr2::req_headers("Content-Type" = "application/json", "Accept" = "application/json") |>
  httr2::req_body_json(payload, auto_unbox = TRUE) |>
  httr2::req_perform()

cat("Predict status:", httr2::resp_status(predict_resp), "\n")
cat(httr2::resp_body_string(predict_resp), "\n")
