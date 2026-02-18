predict_ensemble_all_heads <- function(new_data, ensemble_bundle) {
    b1 <- ensemble_bundle$bundles$type1
    b2 <- ensemble_bundle$bundles$type2
    b3 <- ensemble_bundle$bundles$type3

    n <- nrow(new_data)
    out <- matrix(NA_real_, nrow = n, ncol = 0)

    collect_head_probs <- function(models, predict_fn, n_expected) {
        if (length(models) == 0) {
            return(matrix(numeric(0), nrow = n_expected, ncol = 0))
        }

        probs <- lapply(models, predict_fn)
        lens <- vapply(probs, length, integer(1))
        if (any(lens != n_expected)) {
            stop("Head prediction length mismatch. Expected ", n_expected, " rows.")
        }

        mat <- do.call(cbind, probs)
        if (is.null(dim(mat))) {
            mat <- matrix(mat, nrow = n_expected, ncol = length(models))
        }
        mat
    }

    # Type 1 heads
    p1 <- collect_head_probs(b1$models, function(m) {
        predict_type1_manual(new_data = new_data, preprocess = b1$preprocess, model_minimal = m)
    }, n_expected = n)
    out <- cbind(out, p1)

    # Type 2 heads
    p2 <- collect_head_probs(b2$models, function(m) {
        predict_type2_manual(new_data = new_data, preprocess = b2$preprocess, model_minimal = m)
    }, n_expected = n)
    out <- cbind(out, p2)

    # Type 3 heads
    p3 <- collect_head_probs(b3$models, function(m) {
        predict_type3_manual(new_data = new_data, preprocess = b3$preprocess, model_minimal = m)
    }, n_expected = n)
    out <- cbind(out, p3)

    colnames(out) <- c(
        paste0("head_", b1$head_indices),
        paste0("head_", b2$head_indices),
        paste0("head_", b3$head_indices)
    )

    out
}

extract_preprocess_constants_type1 <- function(head_path) {
    head <- readRDS(head_path)

    mold <- workflows::extract_mold(head$model)
    rec <- mold$blueprint$recipe

    if (is.null(rec$steps) || length(rec$steps) == 0) {
        stop("Recipe steps not found. Did you extract the right blueprint$recipe?")
    }

    step_med <- rec$steps[[1]]
    medians <- step_med$medians

    step_norm <- rec$steps[[6]]
    means <- step_norm$means
    sds <- step_norm$sds

    list(
        medians = medians,
        means = means,
        sds = sds
    )
}

extract_preprocess_constants_type2 <- function(head_path) {
    head <- readRDS(head_path)

    mold <- workflows::extract_mold(head$model)
    rec <- mold$blueprint$recipe

    medians <- rec$steps[[1]]$medians

    step_norm <- rec$steps[[length(rec$steps)]]
    means <- step_norm$means
    sds <- step_norm$sds

    # Model feature names
    model_min <- strip_model_type1_glmnetpredict(head_path)
    features <- model_min$features


    ns_steps <- Filter(function(st) inherits(st, "step_ns"), rec$steps)
    if (length(ns_steps) == 0) stop("No step_ns found in recipe; not Type 2?")

    ns_specs <- list()
    for (st in ns_steps) {
        objs <- st$objects
        if (is.null(objs) || length(objs) == 0) stop("Found step_ns but no trained objects; recipe not trained?")

        for (var in names(objs)) {
            obj <- objs[[var]]
            at <- attributes(obj)

            expected_cols <- grep(paste0("^", var, "_ns_"), features, value = TRUE)
            if (length(expected_cols) == 0) stop("No expected spline columns found in model for var: ", var)

            ns_specs[[var]] <- list(
                var = var,
                knots = at$knots,
                boundary_knots = at$Boundary.knots,
                intercept = isTRUE(at$intercept),
                df = at$df,
                expected_cols = expected_cols
            )
        }
    }

    list(
        medians = medians,
        means = means,
        sds = sds,
        ns_specs = ns_specs,
        features = features
    )
}

extract_preprocess_constants_type3 <- function(head_path) {
    head <- readRDS(head_path)
    mold <- workflows::extract_mold(head$model)
    rec <- mold$blueprint$recipe

    step_med <- rec$steps[[1]]
    medians <- step_med$medians

    step_norm <- rec$steps[[length(rec$steps)]]
    means <- step_norm$means
    sds <- step_norm$sds

    list(
        medians = medians,
        means = means,
        sds = sds,
        features = names(means)
    )
}

strip_model_type1_glmnetpredict <- function(head_path) {
    head <- readRDS(head_path)

    penalty <- rlang::eval_tidy(head$model$fit$actions$model$spec$args$penalty)

    glmnet_fit <- head$model$fit$fit$fit
    lambda <- glmnet_fit$lambda

    lam_min <- min(lambda)
    lam_max <- max(lambda)

    clipped <- FALSE
    penalty_used <- penalty

    if (penalty > lam_max) {
        penalty_used <- lam_max
        clipped <- TRUE
    }
    if (penalty < lam_min) {
        penalty_used <- lam_min
        clipped <- TRUE
    }

    # IMPORTANT: use stats::predict (generic), not glmnet::predict
    coef_mat <- as.matrix(stats::predict(glmnet_fit, s = penalty_used, type = "coefficients"))

    intercept <- as.numeric(coef_mat["(Intercept)", 1])
    features <- setdiff(rownames(coef_mat), "(Intercept)")
    coef_vec <- as.numeric(coef_mat[features, 1])

    list(
        intercept = intercept,
        coefficients = coef_vec,
        features = features,
        penalty = penalty,
        penalty_used = penalty_used,
        penalty_clipped = clipped,
        lambda_min = lam_min,
        lambda_max = lam_max
    )
}

strip_model_type2_glmnetpredict <- function(head_path) {
    head <- readRDS(head_path)

    penalty <- rlang::eval_tidy(head$model$fit$actions$model$spec$args$penalty)

    glmnet_fit <- head$model$fit$fit$fit
    lambda <- glmnet_fit$lambda

    lam_min <- min(lambda)
    lam_max <- max(lambda)

    clipped <- FALSE
    penalty_used <- penalty
    if (penalty > lam_max) {
        penalty_used <- lam_max
        clipped <- TRUE
    }
    if (penalty < lam_min) {
        penalty_used <- lam_min
        clipped <- TRUE
    }

    coef_mat <- as.matrix(stats::predict(glmnet_fit, s = penalty_used, type = "coefficients"))

    intercept <- as.numeric(coef_mat["(Intercept)", 1])
    features <- setdiff(rownames(coef_mat), "(Intercept)")
    coef_vec <- as.numeric(coef_mat[features, 1])

    list(
        intercept = intercept,
        coefficients = coef_vec,
        features = features,
        penalty = penalty,
        penalty_used = penalty_used,
        penalty_clipped = clipped,
        lambda_min = lam_min,
        lambda_max = lam_max
    )
}

strip_model_type3_rpart <- function(head_path) {
    head <- readRDS(head_path)
    parsnip_fit <- head$model$fit$fit
    rpart_fit <- parsnip_fit$fit
    list(fit = rpart_fit)
}

manual_type3_predictors_df <- function(new_data, preprocess) {
    df <- as.data.frame(new_data)

    base_vars <- c(
        "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
        "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
    )

    missing_base <- setdiff(base_vars, names(df))
    if (length(missing_base) > 0) {
        stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
    }

    for (v in base_vars) {
        df[[v]] <- as.numeric(as.character(df[[v]]))
    }

    for (v in base_vars) {
        med <- preprocess$medians[[v]]
        if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
        na_idx <- is.na(df[[v]])
        if (any(na_idx)) df[[v]][na_idx] <- med
    }

    df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
    df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
    df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]

    feature_names <- preprocess$features
    missing_feats <- setdiff(feature_names, names(df))
    if (length(missing_feats) > 0) {
        stop("Missing engineered/expected features: ", paste(missing_feats, collapse = ", "))
    }

    Xdf <- df[, feature_names, drop = FALSE]

    for (v in feature_names) {
        mu <- preprocess$means[[v]]
        sd <- preprocess$sds[[v]]
        if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
        Xdf[[v]] <- (Xdf[[v]] - mu) / sd
    }

    Xdf
}

manual_type1_matrix <- function(new_data, preprocess, feature_names) {
    df <- as.data.frame(new_data)

    base_vars <- c(
        "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
        "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
    )

    missing_base <- setdiff(base_vars, names(df))
    if (length(missing_base) > 0) {
        stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
    }

    for (v in base_vars) {
        df[[v]] <- as.numeric(as.character(df[[v]]))
    }

    for (v in base_vars) {
        med <- preprocess$medians[[v]]
        if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
        na_idx <- is.na(df[[v]])
        if (any(na_idx)) df[[v]][na_idx] <- med
    }

    df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
    df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
    df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]

    Xdf <- df[, feature_names, drop = FALSE]

    for (v in feature_names) {
        mu <- preprocess$means[[v]]
        sd <- preprocess$sds[[v]]
        if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
        Xdf[[v]] <- (Xdf[[v]] - mu) / sd
    }

    as.matrix(Xdf)
}

manual_type2_matrix <- function(new_data, preprocess, feature_names = preprocess$features) {
    df <- as.data.frame(new_data)

    base_vars <- c(
        "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
        "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
    )

    missing_base <- setdiff(base_vars, names(df))
    if (length(missing_base) > 0) {
        stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
    }

    for (v in base_vars) df[[v]] <- as.numeric(as.character(df[[v]]))

    for (v in base_vars) {
        med <- preprocess$medians[[v]]
        if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
        na_idx <- is.na(df[[v]])
        if (any(na_idx)) df[[v]][na_idx] <- med
    }

    # step_mutate
    df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
    df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
    df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]

    # step_ns expansions
    for (var in names(preprocess$ns_specs)) {
        spec <- preprocess$ns_specs[[var]]

        basis <- splines::ns(
            df[[var]],
            knots = spec$knots,
            Boundary.knots = spec$boundary_knots,
            intercept = isTRUE(spec$intercept),
            df = spec$df
        )

        if (ncol(basis) != length(spec$expected_cols)) {
            stop(
                "Spline column count mismatch for ", var,
                ": basis=", ncol(basis), " expected=", length(spec$expected_cols)
            )
        }

        colnames(basis) <- spec$expected_cols
        for (j in seq_len(ncol(basis))) {
            df[[colnames(basis)[j]]] <- basis[, j]
        }

        if (!(var %in% feature_names) && (var %in% names(df))) {
            df[[var]] <- NULL
        }
    }

    # subset in exact model order
    Xdf <- df[, feature_names, drop = FALSE]

    # normalize
    for (v in feature_names) {
        mu <- preprocess$means[[v]]
        sd <- preprocess$sds[[v]]
        if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
        Xdf[[v]] <- (Xdf[[v]] - mu) / sd
    }

    as.matrix(Xdf)
}

predict_type1_minimal_from_X <- function(X, model_minimal) {
    eta <- as.numeric(model_minimal$intercept) + as.vector(X %*% model_minimal$coefficients)
    1 / (1 + exp(-eta))
}

predict_type1_manual <- function(new_data, preprocess, model_minimal) {
    X <- manual_type1_matrix(new_data, preprocess, model_minimal$features)
    predict_type1_minimal_from_X(X, model_minimal)
}

predict_type2_manual <- function(new_data, preprocess, model_minimal) {
    X <- manual_type2_matrix(new_data, preprocess, model_minimal$features)
    eta <- as.numeric(model_minimal$intercept) + as.vector(X %*% model_minimal$coefficients)
    1 / (1 + exp(-eta))
}

predict_type3_manual <- function(new_data, preprocess, model_minimal) {
    Xdf <- manual_type3_matrix(new_data, preprocess)

    p <- stats::predict(model_minimal$fit, newdata = Xdf, type = "prob")
    if (!is.matrix(p) || !"yes" %in% colnames(p)) {
        stop("Unexpected rpart probability output; expected a matrix with column 'yes'.")
    }
    as.numeric(p[, "yes"])
}

predict_minimal <- function(new_data, preprocess, model_minimal) {
    X <- manual_type2_matrix(new_data, preprocess)
    eta <- model_minimal$intercept + as.vector(X %*% model_minimal$coefficients)
    1 / (1 + exp(-eta))
}

extract_preprocess_constants_type3 <- function(head_path) {
    head <- readRDS(head_path)

    mold <- workflows::extract_mold(head$model)
    rec <- mold$blueprint$recipe

    if (is.null(rec$steps) || length(rec$steps) == 0) {
        stop("Recipe steps not found. Did you extract the right blueprint$recipe?")
    }

    step_med <- rec$steps[[1]]
    medians <- step_med$medians

    step_norm <- rec$steps[[6]]
    means <- step_norm$means
    sds <- step_norm$sds

    list(
        medians = medians,
        means = means,
        sds = sds
    )
}

strip_model_type3_rpart <- function(head_path) {
    head <- readRDS(head_path)


    rpart_fit <- head$model$fit$fit$fit
    if (!inherits(rpart_fit, "rpart")) {
        stop("Expected an rpart fit, got: ", paste(class(rpart_fit), collapse = ", "))
    }

    list(
        engine = "rpart",
        fit = rpart_fit
    )
}

manual_type3_matrix <- function(new_data, preprocess) {
    df <- as.data.frame(new_data)

    base_vars <- c(
        "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
        "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long", "oxy.ra"
    )

    missing_base <- setdiff(base_vars, names(df))
    if (length(missing_base) > 0) {
        stop("Missing required columns in new_data: ", paste(missing_base, collapse = ", "))
    }

    for (v in base_vars) {
        df[[v]] <- as.numeric(as.character(df[[v]]))
    }

    for (v in base_vars) {
        med <- preprocess$medians[[v]]
        if (is.null(med) || length(med) != 1) stop("Median not found for variable: ", v)
        na_idx <- is.na(df[[v]])
        if (any(na_idx)) df[[v]][na_idx] <- med
    }

    df[["oxy.ra"]] <- log(df[["oxy.ra"]] + 1e-06)
    df[["age_x_rr"]] <- df[["age.months"]] * df[["rr.all"]]
    df[["age_x_hr"]] <- df[["age.months"]] * df[["hr.all"]]

    feature_names <- c(
        "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
        "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long",
        "oxy.ra", "age_x_rr", "age_x_hr"
    )

    Xdf <- df[, feature_names, drop = FALSE]

    for (v in feature_names) {
        mu <- preprocess$means[[v]]
        sd <- preprocess$sds[[v]]
        if (is.null(mu) || is.null(sd)) stop("Mean/SD not found for feature: ", v)
        Xdf[[v]] <- (Xdf[[v]] - mu) / sd
    }

    Xdf
}

predict_type3_manual <- function(new_data, preprocess, model_minimal) {
    Xdf <- manual_type3_matrix(new_data, preprocess)

    p <- stats::predict(model_minimal$fit, newdata = Xdf, type = "prob")
    if (!is.matrix(p) || !"yes" %in% colnames(p)) {
        stop("Unexpected rpart probability output; expected a matrix with column 'yes'.")
    }
    as.numeric(p[, "yes"])
}
