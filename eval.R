library(dplyr)

qwk_score <- function(true, pred) {
  df <- data.frame(true = as.numeric(true), pred = as.numeric(pred))
  
  # If either column has <2 unique values, QWK is undefined → return NA
  if (length(unique(df$true)) < 2 || length(unique(df$pred)) < 2) {
    return(NA_real_)
  }
  
  out <- tryCatch(
    irr::kappa2(df, weight = "squared")$value,
    error = function(e) NA_real_
  )
  return(out)
}


log_loss <- function(true, prob_df) {
  y <- as.numeric(true)   # converts NoDiabetes=1, Pre=2, Diabetes=3
  probs <- as.matrix(prob_df)
  eps <- 1e-15
  probs <- pmax(pmin(probs, 1 - eps), eps)
  -mean(log(probs[cbind(seq_along(y), y)]))
}

mae_ord <- function(true, pred) {
  mean(abs(as.numeric(true) - as.numeric(pred)))
}

macro_auc <- function(true, prob_df) {
  classes <- levels(true)
  aucs <- c()
  
  for (cls in classes) {
    binary_true <- ifelse(true == cls, 1, 0)
    roc_obj <- pROC::roc(binary_true, prob_df[[cls]], quiet = TRUE)
    aucs <- c(aucs, pROC::auc(roc_obj))
  }
  return(mean(aucs))
}
