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
    has_model <- file.exists(file.path(dir, "models", "day1_bundle.rds"))
    has_predict <- file.exists(file.path(dir, "R", "predict.r"))
    if (isTRUE(has_model) && isTRUE(has_predict)) {
      return(dir)
    }
  }

  stop(
    "Could not locate API directory containing models/day1_bundle.rds and R/predict.r. Searched: ",
    paste(candidates, collapse = ", ")
  )
}

api_dir <- find_api_dir()
model_path <- file.path(api_dir, "models", "day1_bundle.rds")

if (!file.exists(model_path)) {
  stop("Missing day1 bundle at: ", model_path)
}

bundle_day1 <- readRDS(model_path)

source(file.path(api_dir, "R", "predict.r"))

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

required_input_fields <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
)

level_labels <- c(
  L1 = "Mechanical ventilation, inotropes, or renal replacement therapy",
  L2 = "CPAP or IV fluid bolus",
  L3 = "ICU admission with clinical reason",
  L4 = "O2 via face or nasal cannula",
  L5 = "Non-bolused IV fluids"
)

predict_ensemble_summary <- function(new_data, ensemble_bundle, vote_threshold = 0.5) {
  pmat <- predict_ensemble_all_heads(new_data, ensemble_bundle)

  mean_prob <- rowMeans(pmat, na.rm = TRUE)
  n_votes <- rowSums(pmat > vote_threshold, na.rm = TRUE)
  n_heads <- rowSums(!is.na(pmat))
  vote_frac <- ifelse(n_heads > 0, n_votes / n_heads, NA_real_)
  ensemble_yes <- n_votes > (n_heads / 2)
  pct_heads_used <- (n_heads / 120) * 100

  data.frame(
    mean_predicted_probability = mean_prob,
    votes_exceeding_threshold = n_votes,
    pct_heads_used = pct_heads_used,
    votes_above_threshold = vote_frac,
    predicted_treatment_by_majority_vote = ensemble_yes,
    stringsAsFactors = FALSE
  )
}

predict_day1_long <- function(new_data, bundle_day1, levels = NULL, vote_threshold = NULL) {
  available <- names(bundle_day1$levels)
  lvls <- parse_levels(levels, available)
  vt <- vote_threshold %||% (bundle_day1$meta$vote_threshold_default %||% 0.5)

  res <- lapply(lvls, function(lvl) {
    ens <- bundle_day1$levels[[lvl]]
    df <- predict_ensemble_summary(new_data, ens, vote_threshold = vt)
    df$level <- unname(level_labels[[lvl]] %||% lvl)
    df[, c("level", setdiff(names(df), "level")), drop = FALSE]
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

predict_day1_wide <- function(new_data, bundle_day1, levels = NULL, vote_threshold = NULL) {
  long <- predict_day1_long(new_data, bundle_day1, levels = levels, vote_threshold = vote_threshold)
  split_df <- split(long, long$level)
  out <- lapply(split_df, function(df) df[setdiff(names(df), "level")])
  out
}

#* @apiTitle Sepsis Flow Day-1 API
#* @apiDescription Day-1 treatment ensemble predictions (L1–L5) with mean probability and voting summaries.

#* Health check
#* @get /health
function() {
  list(
    status = "ok",
    model_path = basename(model_path),
    levels = names(bundle_day1$levels),
    created_utc = bundle_day1$created_utc %||% NA_character_
  )
}

#* Predict Day-1 ensemble summaries for one or more patients
#* @post /predict/day1
#* @param vote_threshold:double Optional. Threshold used for counting votes (default from bundle meta or 0.5)
#* @param format:string Optional. "long" or "wide" (default "long")
#* @body raw JSON payload: object (one patient) or array of objects (many patients)
#* @serializer json list(auto_unbox = TRUE, digits = 10)
function(req, res, vote_threshold = NULL, format = "long") {
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

  out <- tryCatch(
    {
      if (format == "wide") {
        predict_day1_wide(df, bundle_day1, levels = levels, vote_threshold = vt)
      } else {
        predict_day1_long(df, bundle_day1, levels = levels, vote_threshold = vt)
      }
    },
    error = function(e) e
  )

  if (inherits(out, "error")) {
    res$status <- 500
    return(list(error = out$message))
  }

  out
}
