suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

find_api_dir <- function() {
  script_ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- unique(c(
    if (!is.null(script_ofile)) dirname(script_ofile) else NULL,
    "api",
    ".",
    "/app"
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)

  for (dir in candidates) {
    has_model <- file.exists(file.path(dir, "models", "day2_bundle.rds"))
    has_predict <- file.exists(file.path(dir, "R", "predict.r"))
    if (isTRUE(has_model) && isTRUE(has_predict)) {
      return(dir)
    }
  }

  stop(
    "Could not locate API directory containing models/day2_bundle.rds and R/predict.r. Searched: ",
    paste(candidates, collapse = ", ")
  )
}

api_dir <- find_api_dir()
model_path <- file.path(api_dir, "models", "day2_bundle.rds")

if (!file.exists(model_path)) {
  stop("Missing day2 bundle at: ", model_path)
}

bundle_day2 <- readRDS(model_path)

source(file.path(api_dir, "R", "predict.r"))

find_prevalence_path <- function(api_dir) {
  env_path <- trimws(Sys.getenv("PREVALENCE_TABLE_PATH", unset = ""))
  if (nzchar(env_path)) {
    resolved <- normalizePath(env_path, winslash = "/", mustWork = FALSE)
    if (!file.exists(resolved)) {
      stop("PREVALENCE_TABLE_PATH does not exist: ", resolved)
    }
    return(resolved)
  }

  candidates <- c(
    file.path(api_dir, "prevalence", "prevalence_all_nested.rds"),
    file.path(api_dir, "..", "..", "..", "strata prevalence adjustment", "prevalence_all_nested.rds"),
    file.path("/app", "prevalence", "prevalence_all_nested.rds"),
    file.path(getwd(), "strata prevalence adjustment", "prevalence_all_nested.rds")
  )
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) {
    stop(
      "Could not locate prevalence_all_nested.rds. Searched: ",
      paste(candidates, collapse = ", ")
    )
  }

  found[[1]]
}

prevalence_path <- find_prevalence_path(api_dir)
prevalence_nested <- readRDS(prevalence_path)
required_prevalence_scopes <- c("whole", "country", "inpatient_status", "country_x_inpatient")
if (!all(required_prevalence_scopes %in% names(prevalence_nested))) {
  stop(
    "Prevalence object missing required scopes. Found: ",
    paste(names(prevalence_nested), collapse = ", ")
  )
}

as_clean_character <- function(x) {
  out <- as.character(x)
  out <- trimws(out)
  out
}

is_blank_value <- function(x) {
  x_chr <- as_clean_character(x)
  is.na(x_chr) | !nzchar(x_chr)
}

normalize_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as_clean_character(x)))
}

coerce_stratum_vector <- function(x, n_rows, field_name) {
  if (is.null(x)) return(rep(NA_character_, n_rows))
  vals <- as_clean_character(x)
  if (length(vals) == 1) return(rep(vals, n_rows))
  if (length(vals) == n_rows) return(vals)
  stop(field_name, " must have length 1 or ", n_rows, " values.")
}

extract_strata_defaults <- function(payload, query_country = NULL, query_inpatient_status = NULL) {
  payload_strata <- if (is.list(payload$strata)) payload$strata else list()
  list(
    country = query_country %||% payload$country %||% payload_strata$country %||% NULL,
    inpatient_status = query_inpatient_status %||% payload$inpatient_status %||% payload_strata$inpatient_status %||% NULL
  )
}

resolve_request_strata <- function(df, payload, query_country = NULL, query_inpatient_status = NULL) {
  n_rows <- nrow(df)
  defaults <- extract_strata_defaults(
    payload = payload,
    query_country = query_country,
    query_inpatient_status = query_inpatient_status
  )

  default_country <- coerce_stratum_vector(defaults$country, n_rows, "country")
  default_inpatient <- coerce_stratum_vector(defaults$inpatient_status, n_rows, "inpatient_status")

  country_raw <- if ("country" %in% names(df)) as_clean_character(df$country) else rep(NA_character_, n_rows)
  inpatient_raw <- if ("inpatient_status" %in% names(df)) as_clean_character(df$inpatient_status) else rep(NA_character_, n_rows)

  country_missing <- is_blank_value(country_raw)
  inpatient_missing <- is_blank_value(inpatient_raw)

  country_raw[country_missing] <- default_country[country_missing]
  inpatient_raw[inpatient_missing] <- default_inpatient[inpatient_missing]

  list(country = country_raw, inpatient_status = inpatient_raw)
}

sample_outcome_key <- names(prevalence_nested$country)[1]
available_countries <- unique(as_clean_character(prevalence_nested$country[[sample_outcome_key]]$stratum))
available_inpatient_status <- unique(as_clean_character(prevalence_nested$inpatient_status[[sample_outcome_key]]$stratum))
country_lookup <- setNames(available_countries, normalize_key(available_countries))
inpatient_lookup <- setNames(available_inpatient_status, normalize_key(available_inpatient_status))

normalize_country_values <- function(country_raw) {
  out <- rep(NA_character_, length(country_raw))
  provided <- !is_blank_value(country_raw)
  if (!any(provided)) return(out)
  keys <- normalize_key(country_raw[provided])
  out[provided] <- unname(country_lookup[keys])
  out
}

normalize_inpatient_status_values <- function(inpatient_raw) {
  out <- rep(NA_character_, length(inpatient_raw))
  provided <- !is_blank_value(inpatient_raw)
  if (!any(provided)) return(out)

  keys <- normalize_key(inpatient_raw[provided])
  vals <- rep(NA_character_, length(keys))

  inpatient_keys <- c("1", "true", "yes", "y", "inpatient", "in", "ip", "i")
  outpatient_keys <- c("0", "false", "no", "n", "outpatient", "out", "op", "o")
  vals[keys %in% inpatient_keys] <- "Inpatient"
  vals[keys %in% outpatient_keys] <- "Outpatient"

  unresolved <- is.na(vals)
  if (any(unresolved)) {
    vals[unresolved] <- unname(inpatient_lookup[keys[unresolved]])
  }

  out[provided] <- vals
  out
}

adjust_probability_for_prevalence <- function(p_50_50, prevalence) {
  p <- as.numeric(p_50_50)
  pi <- as.numeric(prevalence)
  out <- rep(NA_real_, length(p))

  finite <- is.finite(p) & is.finite(pi)
  if (!any(finite)) return(out)

  p <- pmin(pmax(p, 0), 1)
  pi <- pmin(pmax(pi, 0), 1)

  out[finite & pi <= 0] <- 0
  out[finite & pi >= 1] <- 1

  idx_mid <- finite & pi > 0 & pi < 1
  out[idx_mid & p <= 0] <- 0
  out[idx_mid & p >= 1] <- 1

  idx_calc <- idx_mid & p > 0 & p < 1
  if (any(idx_calc)) {
    odds <- p[idx_calc] / (1 - p[idx_calc])
    prior_odds <- pi[idx_calc] / (1 - pi[idx_calc])
    adjusted_odds <- odds * prior_odds
    out[idx_calc] <- adjusted_odds / (1 + adjusted_odds)
  }

  out
}

compute_prevalence_adjustment <- function(
    p_50_50,
    threshold_50_50,
    outcome_key,
    country_raw,
    inpatient_raw) {
  n_rows <- length(p_50_50)
  country_raw <- coerce_stratum_vector(country_raw, n_rows, "country")
  inpatient_raw <- coerce_stratum_vector(inpatient_raw, n_rows, "inpatient_status")
  threshold_50_50 <- as.numeric(threshold_50_50)[1]
  if (!is.finite(threshold_50_50)) threshold_50_50 <- NA_real_

  if (!(outcome_key %in% names(prevalence_nested$whole))) {
    stop("Unsupported outcome key for prevalence lookup: ", outcome_key)
  }

  country_norm <- normalize_country_values(country_raw)
  inpatient_norm <- normalize_inpatient_status_values(inpatient_raw)

  provided_country <- !is_blank_value(country_raw)
  provided_inpatient <- !is_blank_value(inpatient_raw)

  invalid_country <- provided_country & is.na(country_norm)
  if (any(invalid_country)) {
    bad_values <- unique(as_clean_character(country_raw[invalid_country]))
    stop(
      "Unsupported country value(s): ",
      paste(bad_values, collapse = ", "),
      ". Allowed values: ",
      paste(available_countries, collapse = ", ")
    )
  }

  invalid_inpatient <- provided_inpatient & is.na(inpatient_norm)
  if (any(invalid_inpatient)) {
    bad_values <- unique(as_clean_character(inpatient_raw[invalid_inpatient]))
    stop(
      "Unsupported inpatient_status value(s): ",
      paste(bad_values, collapse = ", "),
      ". Allowed values: ",
      paste(available_inpatient_status, collapse = ", "),
      " (or 1/0, yes/no, inpatient/outpatient)."
    )
  }

  has_any_strata <- provided_country | provided_inpatient
  if (!any(has_any_strata)) {
    return(list(
      include_adjusted = FALSE,
      data = data.frame(
        p_50_50 = p_50_50,
        t_50_50 = rep(threshold_50_50, n_rows),
        stringsAsFactors = FALSE
      )
    ))
  }

  prevalence <- rep(NA_real_, n_rows)
  prevalence_scope <- rep(NA_character_, n_rows)
  prevalence_stratum <- rep(NA_character_, n_rows)

  for (i in which(has_any_strata)) {
    if (provided_country[i] && provided_inpatient[i]) {
      scope_i <- "country_x_inpatient"
      stratum_i <- paste0(country_norm[i], ".", ifelse(inpatient_norm[i] == "Inpatient", "I", "O"))
    } else if (provided_country[i]) {
      scope_i <- "country"
      stratum_i <- country_norm[i]
    } else {
      scope_i <- "inpatient_status"
      stratum_i <- inpatient_norm[i]
    }

    table_i <- prevalence_nested[[scope_i]][[outcome_key]]
    idx <- match(stratum_i, as_clean_character(table_i$stratum))
    if (is.na(idx)) {
      stop(
        "No prevalence entry for stratum '", stratum_i,
        "' in scope '", scope_i,
        "' and outcome '", outcome_key, "'."
      )
    }

    prevalence_scope[i] <- scope_i
    prevalence_stratum[i] <- stratum_i
    prevalence[i] <- as.numeric(table_i$prevalence[[idx]])
  }

  p_adj <- rep(NA_real_, n_rows)
  t_adj <- rep(NA_real_, n_rows)
  idx_adj <- which(has_any_strata)
  p_adj[idx_adj] <- adjust_probability_for_prevalence(p_50_50[idx_adj], prevalence[idx_adj])
  t_adj[idx_adj] <- adjust_probability_for_prevalence(rep(threshold_50_50, length(idx_adj)), prevalence[idx_adj])

  list(
    include_adjusted = TRUE,
    data = data.frame(
      p_50_50 = p_50_50,
      t_50_50 = rep(threshold_50_50, n_rows),
      p_adj = p_adj,
      t_adj = t_adj,
      prevalence = prevalence,
      prevalence_scope = prevalence_scope,
      prevalence_stratum = prevalence_stratum,
      stringsAsFactors = FALSE
    )
  )
}

as_df_payload <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.list(x) && length(x) > 0 && !is.null(names(x))) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }

  if (is.list(x) && length(x) > 0 && is.null(names(x))) {
    rows <- lapply(x, function(row) {
      if (!is.list(row) || is.null(names(row))) stop("Each row must be a named object.")
      as.data.frame(row, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }

  stop("Invalid payload. Provide a JSON object (single row) or an array of objects (multiple rows).")
}

parse_levels <- function(levels, available) {
  if (is.null(levels) || length(levels) == 0) {
    return(available)
  }
  levels <- as.character(levels)
  levels <- intersect(levels, available)
  if (length(levels) == 0) stop("No valid levels selected. Available: ", paste(available, collapse = ", "))
  levels
}

collect_required_input_fields <- function(bundle) {
  core_fields <- c(
    "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
    "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
  )

  level_fields <- unique(unlist(lapply(bundle$levels, function(level_bundle) {
    unique(c(
      names(level_bundle$bundles$type1$preprocess$medians),
      names(level_bundle$bundles$type2$preprocess$medians),
      names(level_bundle$bundles$type3$preprocess$medians)
    ))
  })))

  c(core_fields, sort(setdiff(level_fields, core_fields)))
}

required_input_fields <- collect_required_input_fields(bundle_day2)

level_labels <- c(
  L1 = "Mechanical ventilation, inotropes, or renal replacement therapy",
  L2 = "CPAP or IV fluid bolus",
  L3 = "ICU admission with clinical reason",
  L4 = "O2 via face or nasal cannula",
  L5 = "Non-bolused IV fluids"
)

level_to_outcome_key_day2 <- c(
  L1 = "LEVEL1_TREATMENTS_D2_SAFE_0",
  L2 = "LEVEL2_TREATMENTS_D2_SAFE_0",
  L3 = "LEVEL3_TREATMENTS_D2_SAFE_0",
  L4 = "LEVEL4_TREATMENTS_D2_SAFE_0",
  L5 = "LEVEL5_TREATMENTS_D2_SAFE_0"
)

predict_ensemble_summary <- function(
    new_data,
    ensemble_bundle,
    vote_threshold = 0.5,
    outcome_key,
    strata_country = NULL,
    strata_inpatient_status = NULL) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)

  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  ensemble_yes <- n_votes > (n_heads / 2)
  pct_heads_used <- (n_heads / 120) * 100

  prevalence_adjustment <- compute_prevalence_adjustment(
    p_50_50 = mean_prob,
    threshold_50_50 = vote_threshold,
    outcome_key = outcome_key,
    country_raw = strata_country,
    inpatient_raw = strata_inpatient_status
  )

  out <- data.frame(
    mean_predicted_probability = mean_prob,
    votes_exceeding_threshold = n_votes,
    pct_heads_used = pct_heads_used,
    votes_above_threshold = vote_frac,
    predicted_treatment_by_majority_vote = ensemble_yes,
    p_50_50 = prevalence_adjustment$data$p_50_50,
    t_50_50 = prevalence_adjustment$data$t_50_50,
    stringsAsFactors = FALSE
  )

  if (isTRUE(prevalence_adjustment$include_adjusted)) {
    out$p_adj <- prevalence_adjustment$data$p_adj
    out$t_adj <- prevalence_adjustment$data$t_adj
    out$prevalence <- prevalence_adjustment$data$prevalence
    out$prevalence_scope <- prevalence_adjustment$data$prevalence_scope
    out$prevalence_stratum <- prevalence_adjustment$data$prevalence_stratum
  }

  out
}

predict_day2_long <- function(
    new_data,
    bundle_day2,
    levels = NULL,
    vote_threshold = NULL,
    strata_country = NULL,
    strata_inpatient_status = NULL) {
  available <- names(bundle_day2$levels)
  lvls <- parse_levels(levels, available)
  vt <- vote_threshold %||% (bundle_day2$meta$vote_threshold_default %||% 0.5)

  res <- lapply(lvls, function(lvl) {
    outcome_key <- unname(level_to_outcome_key_day2[[lvl]])
    if (!nzchar(outcome_key %||% "")) stop("No prevalence outcome key mapped for level: ", lvl)

    ens <- bundle_day2$levels[[lvl]]
    df <- predict_ensemble_summary(
      new_data,
      ens,
      vote_threshold = vt,
      outcome_key = outcome_key,
      strata_country = strata_country,
      strata_inpatient_status = strata_inpatient_status
    )
    df$level <- unname(level_labels[[lvl]] %||% lvl)
    df[, c("level", setdiff(names(df), "level")), drop = FALSE]
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

predict_day2_wide <- function(
    new_data,
    bundle_day2,
    levels = NULL,
    vote_threshold = NULL,
    strata_country = NULL,
    strata_inpatient_status = NULL) {
  long <- predict_day2_long(
    new_data,
    bundle_day2,
    levels = levels,
    vote_threshold = vote_threshold,
    strata_country = strata_country,
    strata_inpatient_status = strata_inpatient_status
  )
  split_df <- split(long, long$level)
  out <- lapply(split_df, function(df) df[setdiff(names(df), "level")])
  out
}

#* @apiTitle Sepsis Flow Day-2 API
#* @apiDescription Day-2 treatment ensemble predictions (L1–L5) with mean probability and voting summaries.

#* Health check
#* @get /health
function() {
  list(
    status = "ok",
    model_path = basename(model_path),
    prevalence_path = prevalence_path,
    levels = names(bundle_day2$levels),
    created_utc = bundle_day2$created_utc %||% NA_character_,
    required_fields = required_input_fields
  )
}

#* Predict Day-2 ensemble summaries for one or more patients
#* @post /predict/day2
#* @param vote_threshold:double Optional. Threshold used for counting votes (default from bundle meta or 0.5)
#* @param format:string Optional. "long" or "wide" (default "long")
#* @param country:string Optional. Country stratum for prevalence adjustment.
#* @param inpatient_status:string Optional. Inpatient status stratum ("Inpatient"/"Outpatient") for prevalence adjustment.
#* @body raw JSON payload: object (one patient) or array of objects (many patients). Optional strata can be supplied via `country`, `inpatient_status`, or nested `strata`.
#* @serializer json list(auto_unbox = TRUE, digits = 10)
function(req, res, vote_threshold = NULL, format = "long", country = NULL, inpatient_status = NULL) {
  payload <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload)) {
    res$status <- 400
    return(list(error = "Invalid JSON body."))
  }

  levels <- payload$levels %||% NULL
  data_in <- payload$data %||% payload

  df <- tryCatch(as_df_payload(data_in), error = function(e) e)
  if (inherits(df, "error")) {
    res$status <- 400
    return(list(error = df$message))
  }

  missing_fields <- setdiff(required_input_fields, names(df))
  if (length(missing_fields) > 0) {
    res$status <- 400
    return(list(
      error = paste0(
        "Missing required fields: ",
        paste(missing_fields, collapse = ", ")
      ),
      required_fields = required_input_fields
    ))
  }

  format <- tolower(as.character(format %||% "long"))
  if (!(format %in% c("long", "wide"))) {
    res$status <- 400
    return(list(error = "format must be 'long' or 'wide'."))
  }

  vt <- if (is.null(vote_threshold)) NULL else as.numeric(vote_threshold)

  strata <- tryCatch(
    resolve_request_strata(
      df = df,
      payload = payload,
      query_country = country,
      query_inpatient_status = inpatient_status
    ),
    error = function(e) e
  )
  if (inherits(strata, "error")) {
    res$status <- 400
    return(list(error = strata$message))
  }

  out <- tryCatch(
    {
      if (format == "wide") {
        predict_day2_wide(
          df,
          bundle_day2,
          levels = levels,
          vote_threshold = vt,
          strata_country = strata$country,
          strata_inpatient_status = strata$inpatient_status
        )
      } else {
        predict_day2_long(
          df,
          bundle_day2,
          levels = levels,
          vote_threshold = vt,
          strata_country = strata$country,
          strata_inpatient_status = strata$inpatient_status
        )
      }
    },
    error = function(e) e
  )

  if (inherits(out, "error")) {
    is_client_error <- grepl(
      "Unsupported country value\\(s\\)|Unsupported inpatient_status value\\(s\\)|No prevalence entry for stratum",
      out$message
    )
    res$status <- if (isTRUE(is_client_error)) 400 else 500
    return(list(error = out$message))
  }

  out
}
