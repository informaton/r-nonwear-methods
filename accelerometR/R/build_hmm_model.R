# code by paulkr, documentation by paulkr
#' @title Fits a hidden Markov model to accelerometry data.
#' @description \code{\link[mhsmm]{hmmfit}} requires a lot of very
#'    particular inputs before it will run. \code{\link{build_hmm_model}} 
#'    assembles all of the necessary inputs given accelerometry data from
#'    \code{\link{transform_data}} and return the hidden Markov model
#'    from running \code{\link[mhsmm]{hmmfit}} on those inputs.
#' @param data A data frame containing accelerometry data from
#'    \code{\link{transform}}.
#' @return A list of class \code{"hmm"} containing the hidden Markov model fit
#'    to the input data.
#' @keywords internal
build_hmm_model <- function(data) {
  J <- 3
  # initialize the hmm_model with warm-start from past experience
  initial_distribution <- rep(1/J, J)
  state_transition_probs <- matrix(c(
    0.59695306, 0.02180557, 0.3812414,
    0.02685469, 0.90549961, 0.0676457,
    0.15272549, 0.14810543, 0.6991691
  ), nrow = J)
  
  # compute state_means
  predictors <- c("x_axis", "y_axis", "z_axis", "logdetS", "L2_norm", "mSm_norm1", "mSm_norm2", "mDiff", "tDiff", "logSDiff",
                  "theta_x", "theta_y", "theta_z", "logdetT", "logTDiff")
  df <- aggregate(data[, colnames(data) %in% predictors], by = list(data$state),
                  FUN = function(x) mean(x, na.rm = TRUE), simplify = TRUE)
  
  state_means <- list()
  state_means$mu_nonwear <- as.numeric(df[df$Group.1 == "nonwear", -1])
  names(state_means$mu_nonwear) <- colnames(df)[-1]
  state_means$mu_sleep <- as.numeric(df[df$Group.1 == "sleep", -1])
  names(state_means$mu_sleep) <- colnames(df)[-1]
  state_means$mu_wake <- as.numeric(df[df$Group.1 == "wake", -1])
  names(state_means$mu_wake) <- colnames(df)[-1]
  
  # compute state covariances
  state_covs <- lapply(split(data[ , colnames(data) %in% predictors], f = data$state),
                       FUN = function(x) as.matrix(cov(x, use = "complete.obs"), ncol = 15))
  
  # parameters for multivariate normal distributions
  normal_parameters <- list(mu = state_means, sigma = state_covs)
  
  # model initialization
  model_spec <- mhsmm::hmmspec(init = initial_distribution,
                               trans = state_transition_probs,
                               parms.emission = normal_parameters,
                               dens.emission = mhsmm::dmvnorm.hsmm)
  
  # Build the hmm model
  train <- list()
  train$s <- data$state
  train$x <- data[ , colnames(data) %in% predictors]
  train$N <- nrow(data)
  class(train) <- "hsmm.data"
  hmm_model <- mhsmm::hmmfit(train, start.val = model_spec, mstep = mhsmm::mstep.mvnorm)
  hmm_model
}