suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

BASE_URL <- Sys.getenv("SEPSIS_FLOW_API_URL", unset = "http://localhost:8000")

`%||%` <- function(x, y) if (is.null(x)) y else x

get_json <- function(url, timeout_s = 15) {
  resp <- httr2::request(url) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop(sprintf("GET %s failed: HTTP %s\n%s", url, status, httr2::resp_body_string(resp)))
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

post_json <- function(url, payload, timeout_s = 60) {
  resp <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop(sprintf("POST %s failed: HTTP %s\n%s", url, status, httr2::resp_body_string(resp)))
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

wait_for_api <- function(base_url, max_wait_s = 120, poll_interval_s = 3) {
  cat("Waiting for API:", base_url, "\n")
  started <- Sys.time()

  repeat {
    ok <- try(
      {
        resp <- httr2::request(paste0(base_url, "/health")) |>
          httr2::req_timeout(seconds = poll_interval_s) |>
          httr2::req_perform()
        httr2::resp_status(resp) == 200
      },
      silent = TRUE
    )

    if (!inherits(ok, "try-error") && isTRUE(ok)) {
      cat("API is up.\n")
      return(invisible(TRUE))
    }

    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= max_wait_s) {
      stop("API did not become ready within ", max_wait_s, " seconds.")
    }

    cat(".")
    Sys.sleep(poll_interval_s)
  }
}

# Example patient row. Keep names aligned with training features used by bundle_day1.
example_patient <- list(
  age.months = 24,
  sex = 0,
  bgcombyn = 0,
  adm.recent = 0,
  wfaz = -1.1,
  waste = 0,
  stunt = 0,
  cidysymp = 2,
  prior.care = 0,
  travel.time.bin = 0,
  diarrhoeal = 0,
  pneumo = 0,
  sev.pneumo = 0,
  ensapro = 0,
  vomit.all = 0,
  seiz = 0,
  pfacleth = 0,
  not.alert = 0,
  danger.sign = 0,
  hr.all = 120,
  rr.all = 28,
  oxy.ra = 98,
  envhtemp = 37.8,
  crt.long = 0,
  parenteral_screen = 0,
  SIRS_num = 1
)

cat("Base URL:", BASE_URL, "\n")
wait_for_api(BASE_URL)

cat("\nHealth check:\n")
health <- get_json(paste0(BASE_URL, "/health"))
print(health)

cat("\nSingle-row prediction (default format=long):\n")
single_out <- post_json(
  paste0(BASE_URL, "/predict/day1"),
  payload = list(
    data = example_patient,
    levels = c("L1", "L3", "L5")
  )
)
print(single_out)

cat("\nMulti-row prediction (format=wide, custom vote_threshold):\n")
multi_rows <- list(
  example_patient,
  modifyList(example_patient, list(hr.all = 140, rr.all = 34, not.alert = 1, danger.sign = 1))
)

multi_out <- post_json(
  paste0(BASE_URL, "/predict/day1?format=wide&vote_threshold=0.6"),
  payload = list(
    data = multi_rows,
    levels = c("L1", "L2", "L3", "L4", "L5")
  )
)
print(multi_out)

cat("\nDone.\n")
