xgb.plot.shap.1 <- function(data, shap_contrib = NULL, features = NULL, top_n = 1, model = NULL,
                          trees = NULL, target_class = NULL, approxcontrib = FALSE,
                          subsample = NULL, n_col = 1, col = col, pch = '.', 
                          discrete_n_uniq = 5, discrete_jitter = 0.01, ylab = "SHAP", xlab = NULL,
                          plot_NA = TRUE, col_NA = rgb(0.7, 0, 1, 0.6), pch_NA = '.', pos_NA = 1.07,
                          plot_loess = TRUE, col_loess = col_loess, span_loess = 0.5,
                          which = c("1d", "2d"), plot = TRUE, ...) {
  data_list <- xgb.shap.data(
    data = data,
    shap_contrib = shap_contrib,
    features = features,
    top_n = top_n,
    model = model,
    trees = trees,
    target_class = target_class,
    approxcontrib = approxcontrib,
    subsample = subsample,
    max_observations = 100000
  )
  data <- data_list[["data"]]
  shap_contrib <- data_list[["shap_contrib"]]
  features <- colnames(data)
  
  set_plot_dimensions <- function(width_choice, height_choice) {
    options(repr.plot.width=width_choice, repr.plot.height=height_choice)
  }
  set_plot_dimensions(10, 4)
  
  which <- match.arg(which)
  if (which == "2d")
    stop("2D plots are not implemented yet")
  
  if (n_col > length(features)) n_col <- length(features)
  if (plot && which == "1d") {
    op <- par(mfrow = c(ceiling(length(features) / n_col), n_col),
              oma = c(0, 0, 0, 0) + 0.2,
              mar = c(3.5, 3.5, 0, 0) + 0.1,
              mgp = c(1.7, 0.6, 0))
    numf=1
    for (f in features) {
      ord <- order(data[, f])
      x <- data[, f][ord]
      y <- shap_contrib[, f][ord]
      x_lim <- range(x, na.rm = TRUE)
      y_lim <- range(y, na.rm = TRUE)
      xlab_f <- xlab[numf]
      do_na <- plot_NA && anyNA(x)
      numf=numf+1
      if (do_na) {
        x_range <- diff(x_lim)
        loc_na <- min(x, na.rm = TRUE) + x_range * pos_NA
        x_lim <- range(c(x_lim, loc_na))
      }
      x_uniq <- unique(x)
      x2plot <- x
      # add small jitter for discrete features with <= 5 distinct values
      if (length(x_uniq) <= discrete_n_uniq)
      x2plot <- jitter(x, amount = discrete_jitter * min(diff(x_uniq), na.rm = TRUE))
      plot(x2plot, y, pch = pch, xlab = xlab_f, col = col, xlim = x_lim, ylim = y_lim, ylab = ylab, cex=3, ...)
      grid()
      if (plot_loess) {
        # compress x to 3 digits, and mean-aggregate y
        zz <- data.table(x = signif(x, 3), y)[, .(.N, y = mean(y)), x]
        if (nrow(zz) <= 5) {
          lines(zz$x, zz$y, col = col_loess, lwd=2)
        } else {
          lo <- stats::loess(y ~ x, data = zz, weights = zz$N, span = span_loess)
          zz$y_lo <- predict(lo, zz, type = "link")
          lines(zz$x, zz$y_lo, col = col_loess, lwd=2)
        }
      }
      if (do_na) {
        i_na <- which(is.na(x))
        x_na <- rep(loc_na, length(i_na))
        x_na <- jitter(x_na, amount = x_range * 0.01)
        points(x_na, y[i_na], pch = pch_NA, col = col_NA)
      }
    }
    par(op)
  }
  if (plot && which == "2d") {
    # TODO
    warning("Bivariate plotting is currently not available.")
  }
  invisible(list(data = data, shap_contrib = shap_contrib))
  
xgb.shap.data <- function(data, shap_contrib = NULL, features = NULL, top_n = 1, model = NULL,
                            trees = NULL, target_class = NULL, approxcontrib = FALSE,
                            subsample = NULL, max_observations = 100000) {
    if (!is.matrix(data) && !inherits(data, "dgCMatrix"))
      stop("data: must be either matrix or dgCMatrix")
    
    if (is.null(shap_contrib) && (is.null(model) || !inherits(model, "xgb.Booster")))
      stop("when shap_contrib is not provided, one must provide an xgb.Booster model")
    
    if (is.null(features) && (is.null(model) || !inherits(model, "xgb.Booster")))
      stop("when features are not provided, one must provide an xgb.Booster model to rank the features")
    
    if (!is.null(shap_contrib) &&
        (!is.matrix(shap_contrib) || nrow(shap_contrib) != nrow(data) || ncol(shap_contrib) != ncol(data) + 1))
      stop("shap_contrib is not compatible with the provided data")
    
    if (is.character(features) && is.null(colnames(data)))
      stop("either provide `data` with column names or provide `features` as column indices")
    
    if (is.null(model$feature_names) && model$nfeatures != ncol(data))
      stop("if model has no feature_names, columns in `data` must match features in model")
    
    if (!is.null(subsample)) {
      idx <- sample(x = seq_len(nrow(data)), size = as.integer(subsample * nrow(data)), replace = FALSE)
    } else {
      idx <- seq_len(min(nrow(data), max_observations))
    }
    data <- data[idx, ]
    if (is.null(colnames(data))) {
      colnames(data) <- paste0("X", seq_len(ncol(data)))
    }
    
    if (!is.null(shap_contrib)) {
      if (is.list(shap_contrib)) { # multiclass: either choose a class or merge
        shap_contrib <- if (!is.null(target_class)) shap_contrib[[target_class + 1]] else Reduce("+", lapply(shap_contrib, abs))
      }
      shap_contrib <- shap_contrib[idx, ]
      if (is.null(colnames(shap_contrib))) {
        colnames(shap_contrib) <- paste0("X", seq_len(ncol(data)))
      }
    } else {
      shap_contrib <- predict(model, newdata = data, predcontrib = TRUE, approxcontrib = approxcontrib)
      if (is.list(shap_contrib)) { # multiclass: either choose a class or merge
        shap_contrib <- if (!is.null(target_class)) shap_contrib[[target_class + 1]] else Reduce("+", lapply(shap_contrib, abs))
      }
    }
    
    if (is.null(features)) {
      if (!is.null(model$feature_names)) {
        imp <- xgb.importance(model = model, trees = trees)
      } else {
        imp <- xgb.importance(model = model, trees = trees, feature_names = colnames(data))
      }
      top_n <- top_n[1]
      if (top_n < 1 || top_n > 100) stop("top_n: must be an integer within [1, 100]")
      features <- imp$Feature[seq_len(min(top_n, NROW(imp)))]
    }
    if (is.character(features)) {
      features <- match(features, colnames(data))
    }
    
    shap_contrib <- shap_contrib[, features, drop = FALSE]
    data <- data[, features, drop = FALSE]
    
    list(
      data = data,
      shap_contrib = shap_contrib
    )
  }
}

xgb.shap.data <- function(data, shap_contrib = NULL, features = NULL, top_n = 1, model = NULL,
                          trees = NULL, target_class = NULL, approxcontrib = FALSE,
                          subsample = NULL, max_observations = 100000) {
  if (!is.matrix(data) && !inherits(data, "dgCMatrix"))
    stop("data: must be either matrix or dgCMatrix")
  
  if (is.null(shap_contrib) && (is.null(model) || !inherits(model, "xgb.Booster")))
    stop("when shap_contrib is not provided, one must provide an xgb.Booster model")
  
  if (is.null(features) && (is.null(model) || !inherits(model, "xgb.Booster")))
    stop("when features are not provided, one must provide an xgb.Booster model to rank the features")
  
  if (!is.null(shap_contrib) &&
      (!is.matrix(shap_contrib) || nrow(shap_contrib) != nrow(data) || ncol(shap_contrib) != ncol(data) + 1))
    stop("shap_contrib is not compatible with the provided data")
  
  if (is.character(features) && is.null(colnames(data)))
    stop("either provide `data` with column names or provide `features` as column indices")
  
  if (is.null(model$feature_names) && model$nfeatures != ncol(data))
    stop("if model has no feature_names, columns in `data` must match features in model")
  
  if (!is.null(subsample)) {
    idx <- sample(x = seq_len(nrow(data)), size = as.integer(subsample * nrow(data)), replace = FALSE)
  } else {
    idx <- seq_len(min(nrow(data), max_observations))
  }
  data <- data[idx, ]
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("X", seq_len(ncol(data)))
  }
  
  if (!is.null(shap_contrib)) {
    if (is.list(shap_contrib)) { # multiclass: either choose a class or merge
      shap_contrib <- if (!is.null(target_class)) shap_contrib[[target_class + 1]] else Reduce("+", lapply(shap_contrib, abs))
    }
    shap_contrib <- shap_contrib[idx, ]
    if (is.null(colnames(shap_contrib))) {
      colnames(shap_contrib) <- paste0("X", seq_len(ncol(data)))
    }
  } else {
    shap_contrib <- predict(model, newdata = data, predcontrib = TRUE, approxcontrib = approxcontrib)
    if (is.list(shap_contrib)) { # multiclass: either choose a class or merge
      shap_contrib <- if (!is.null(target_class)) shap_contrib[[target_class + 1]] else Reduce("+", lapply(shap_contrib, abs))
    }
  }
  
  if (is.null(features)) {
    if (!is.null(model$feature_names)) {
      imp <- xgb.importance(model = model, trees = trees)
    } else {
      imp <- xgb.importance(model = model, trees = trees, feature_names = colnames(data))
    }
    top_n <- top_n[1]
    if (top_n < 1 || top_n > 100) stop("top_n: must be an integer within [1, 100]")
    features <- imp$Feature[seq_len(min(top_n, NROW(imp)))]
  }
  if (is.character(features)) {
    features <- match(features, colnames(data))
  }
  
  shap_contrib <- shap_contrib[, features, drop = FALSE]
  data <- data[, features, drop = FALSE]
  
  list(
    data = data,
    shap_contrib = shap_contrib
  )
}

