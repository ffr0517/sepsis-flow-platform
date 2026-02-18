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
CSV_PATH <- Sys.getenv("SEPSIS_FLOW_CSV_PATH", unset = "api_demo_two_patients_template.csv")

required_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
)

patients_df <- read.csv(CSV_PATH, check.names = FALSE, stringsAsFactors = FALSE)

if (nrow(patients_df) != 2) {
  stop("Template must contain exactly 2 patient rows. Found: ", nrow(patients_df))
}

missing_cols <- setdiff(required_fields, names(patients_df))
if (length(missing_cols) > 0) {
  stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "))
}

payload <- list(
  data = unname(split(patients_df[, required_fields, drop = FALSE], seq_len(nrow(patients_df)))),
  levels = c("L1", "L2", "L3", "L4", "L5")
)

resp <- httr2::request(paste0(BASE_URL, "/predict/day1?format=wide")) |>
  httr2::req_headers("Content-Type" = "application/json", "Accept" = "application/json") |>
  httr2::req_body_json(payload, auto_unbox = TRUE) |>
  httr2::req_perform()

status <- httr2::resp_status(resp)
if (status >= 400) {
  stop("API error: HTTP ", status, "\n", httr2::resp_body_string(resp))
}

out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
print(out)
