suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# Variable definitions for Day-1 API input:
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

BASE_URL <- Sys.getenv("SEPSIS_FLOW_API_URL", unset = "https://sepsis-flow-d1-api.onrender.com")

required_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
)

predict_single_patient <- function(
    age.months,
    sex,
    adm.recent,
    wfaz,
    cidysymp,
    not.alert,
    hr.all,
    rr.all,
    envhtemp,
    crt.long,
    oxy.ra,
    levels = c("L1", "L2", "L3", "L4", "L5"),
    base_url = BASE_URL
) {
  patient <- list(
    age.months = age.months,
    sex = sex,
    adm.recent = adm.recent,
    wfaz = wfaz,
    cidysymp = cidysymp,
    not.alert = not.alert,
    hr.all = hr.all,
    rr.all = rr.all,
    envhtemp = envhtemp,
    crt.long = crt.long,
    oxy.ra = oxy.ra
  )

  missing_fields <- setdiff(required_fields, names(patient))
  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  resp <- httr2::request(paste0(base_url, "/predict/day1")) |>
    httr2::req_headers("Content-Type" = "application/json", "Accept" = "application/json") |>
    httr2::req_body_json(list(data = patient, levels = levels), auto_unbox = TRUE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop("API error: HTTP ", status, "\n", httr2::resp_body_string(resp))
  }

  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  print(out)
  invisible(out)
}

# Example call: edit values and run this file.
predict_single_patient(
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
  oxy.ra = 98
)
