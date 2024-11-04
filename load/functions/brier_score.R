Brier_LF <- function (object, pre_sp, t_star = -1) {
  if (inherits(object, "coxph")) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- sort(unique(as.vector(obj$y[obj$y[, 2] == 1])))
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    vec_coxph <- predictSurvProb(obj, test_data, t_star0)
    object_coxph <- Surv(test_data$time, test_data$status)
    object <- object_coxph
    pre_sp <- vec_coxph
    t_star <- t_star0
  }
  if (inherits(object, c("rfsrc"))) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- obj$time.interest
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    med_index <- order(abs(distime - t_star0))[1]
    mat_rsf <- predict(obj, test_data)$survival
    vec_rsf <- mat_rsf[, med_index]
    object_rsf <- Surv(test_data$time, test_data$status)
    object <- object_rsf
    pre_sp <- vec_rsf
    t_star <- t_star0
  }
  if (inherits(object, c("survreg"))) {
    obj <- object
    test_data <- pre_sp
    t_star0 <- t_star
    distime <- sort(unique(as.vector(obj$y[obj$y[, 2] == 
                                             1])))
    if (t_star0 <= 0) {
      t_star0 <- median(distime)
    }
    pre_sp <- predictSurvProb2survreg(obj, test_data, t_star0)
    object <- Surv(test_data$time, test_data$status)
    t_star <- t_star0
  }
  if (is.na(t_star)) {
    stop("Cannot calculate Brier Score at NA")
  }
  if (t_star <= 0) {
    t_star <- median(object[, 1][object[, 2] == 1])
  }
  if (length(t_star) != 1) {
    stop("Brier Score can only be calculated at a single time point")
  }
  if (!inherits(object, "Surv")) {
    stop("object is not of class Surv")
  }
  if (any(is.na(object))) {
    stop("The input vector cannot have NA")
  }
  if (any(is.na(pre_sp))) {
    stop("The input probability vector cannot have NA")
  }
  if (length(object) != length(pre_sp)) {
    stop("The prediction survival probability and the survival object have different lengths")
  }
  time <- object[, 1]
  status <- object[, 2]
  t_order <- order(time)
  time <- time[t_order]
  status <- status[t_order]
  pre_sp <- pre_sp[t_order]
  sum_before_t <- 0
  sum_after_t <- 0
  Gtstar <- Gt(object, t_star)
  for (i in c(1:length(time))) {
    if (time[i] < t_star & (status[i] == 1)) {
      Gti <- Gt(object, time[i])
      if (is.na(Gti)) {
        next
      }
      sum_before_t <- sum_before_t + 1/Gti * (pre_sp[i])^2
      next
    }
    if (time[i] >= t_star) {
      if (is.na(Gtstar)) {
        next
      }
      sum_after_t <- sum_after_t + 1/Gtstar * (1 - pre_sp[i])^2
    }
  }
  BSvalue <- (sum_before_t + sum_after_t)/length(time)
  names(BSvalue) <- "Brier Score"
  res <- data.frame(Brier_score = round(BSvalue[[1]],4))
  return(res)
}
