suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# Variable definitions for Day-2 API input:
# patient_name: free-text identifier to track each patient in outputs.
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

BASE_URL <- Sys.getenv("SEPSIS_FLOW_API_URL", unset = "https://sepsis-flow-platform.onrender.com")
CSV_PATH <- Sys.getenv("SEPSIS_FLOW_CSV_PATH", unset = "api_demo_two_patients_template.csv")
OUTPUT_PATH <- Sys.getenv("SEPSIS_FLOW_OUTPUT_PATH", unset = "api_demo_two_patients_results.csv")

required_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra",
  "LEVEL1_TREATMENTS_D1_SAFE_0", "LEVEL2_TREATMENTS_D1_SAFE_0",
  "LEVEL3_TREATMENTS_D1_SAFE_0", "LEVEL4_TREATMENTS_D1_SAFE_0",
  "LEVEL5_TREATMENTS_D1_SAFE_0"
)

patients_df <- read.csv(CSV_PATH, check.names = FALSE, stringsAsFactors = FALSE)

if (nrow(patients_df) < 1) {
  stop("CSV must contain at least 1 patient row. Found: ", nrow(patients_df))
}

if (!("patient_name" %in% names(patients_df))) {
  stop("CSV is missing required column: patient_name")
}

missing_cols <- setdiff(required_fields, names(patients_df))
if (length(missing_cols) > 0) {
  stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "))
}

patient_names <- as.character(patients_df$patient_name)
if (any(!nzchar(patient_names))) {
  stop("CSV has empty patient_name values. Please provide a name for each row.")
}

patient_rows <- lapply(seq_len(nrow(patients_df)), function(i) {
  as.list(patients_df[i, required_fields, drop = FALSE])
})

payload <- list(
  data = unname(patient_rows),
  levels = c("L1", "L2", "L3", "L4", "L5")
)

resp <- httr2::request(paste0(BASE_URL, "/predict/day2?format=wide")) |>
  httr2::req_headers("Content-Type" = "application/json", "Accept" = "application/json") |>
  httr2::req_body_json(payload, auto_unbox = TRUE) |>
  httr2::req_perform()

status <- httr2::resp_status(resp)
if (status >= 400) {
  stop("API error: HTTP ", status, "\n", httr2::resp_body_string(resp))
}

out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
print(out)

rows <- lapply(names(out), function(level_name) {
  level_df <- as.data.frame(out[[level_name]], stringsAsFactors = FALSE)
  if (nrow(level_df) != length(patient_names)) {
    stop("Unexpected response row count for level '", level_name, "'.")
  }
  level_df$level <- level_name
  level_df$patient_name <- patient_names
  level_df[, c("patient_name", "level", setdiff(names(level_df), c("patient_name", "level"))), drop = FALSE]
})

results_df <- do.call(rbind, rows)

results_df
