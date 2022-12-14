#code by paulkr, documentation by paulkr
#' @title Generate state predictions from accelerometry data using a variety of 
#'    statistical models.
#' @description This is a convenience function that takes care of all of the
#'    many idiosyncracies involved in generating predictions from the various
#'    statistical models employed by the accelerometR package.
#' 
#'    The current version of accelerometR only uses mhsmm and randomForest
#'    models, but future versions will implement more.
#' @param data A data frame of accelerometry data. It is required to be in the 
#'    format produced by the transform function in this package.
#' @param hmm_model Either a list of class \code{"hmm"} as produced by a call to 
#'    \code{\link[mhsmm]{hmmfit}} or \code{\link{build}}
#'    containing the hidden Markov model that will be used to generate one set
#'    of predictions, or the string \code{"default"}, or \code{NULL}. If
#'    \code{"default"} is passed, then the function uses the default hmm model
#'    that gets loaded with this package. If \code{NULL}, then no hmm
#'    predictions are produced.
#' @param randomForest_model Either a list of class \code{"randomForest"} as
#'    produced by a call to \code{\link[randomForest]{randomForest}} or 
#'    \code{\link{build}} containing the random Forest model
#'    that will be used to generate one set of predictions, or the string
#'    \code{"default"}, or \code{NULL}. If \code{"default"} is passed, then the
#'    function uses the default random Forest model that gets loaded with this
#'    package. If \code{NULL}, then no random Forest predictions are produced.
#' @return A data frame with one column for the subject id, one column for time,
#'    and one column for each set of state predictions generated.
#' @importFrom stats predict
#' @export
#' @examples
#'    # Using the default settings
#'    data(acclR_data)
#'    predictions1 <- predict_accelerometR(acclR_data)
#' 
#'    # Suppressing the hmm predictions
#'    predictions2 <- predict_accelerometR(acclR_data, hmm_model = NULL)
#' 
#'    # Using a small model built with accelerometR::build()
#'    results <- build(acclR_data, training_fraction = 0.75, ntree = 100,
#'    maxnodes = 10)
#'    predictions3 <- predict_accelerometR(acclR_data,
#'    hmm_model = results$hmm$model,
#'    randomForest_model = results$randomForest$model)
predict_accelerometR <- function(data, hmm_model = "default",
                                 randomForest_model = "default") {
  if (length(hmm_model) == 1) {
    if (hmm_model == "default") {
      hmm_model <- readRDS(system.file("extdata", "default_hmm.RDS",
                                       package = "accelerometR"))
    }
  }
  
  if (length(randomForest_model) == 1) {
    if (randomForest_model == "default") {
      randomForest_model <- readRDS(system.file("extdata",
                                                "default_randomForest.RDS",
                                                package = "accelerometR"))
    }
  }
  
  results <- as.data.frame(matrix(0, nrow = nrow(data), ncol = 2))
  colnames(results) <- c("id", "timestep")
  results$id <- data$id
  results$timestep <- data$timestep
  
  if (!is.null(hmm_model)) {
    predictors <- c("x_axis", "y_axis", "z_axis", "logdetS", "L2_norm",
                    "mSm_norm1", "mSm_norm2", "mDiff", "tDiff", "logSDiff",
                    "theta_x", "theta_y", "theta_z", "logdetT", "logTDiff")
    data_hsmm <- list()
    data_hsmm$s <- data$state
    data_hsmm$x <- data[ , colnames(data) %in% predictors]
    data_hsmm$N <- nrow(data)
    class(data_hsmm) <- "hsmm.data"
    hmm_predictions <- as.factor(mhsmm::predict.hmm(hmm_model,
                                                    newdata = data_hsmm)$s)
    levels(hmm_predictions) <- c("nonwear", "sleep", "wake")
    results$hmm_pred <- hmm_predictions
  }
  
  if (!is.null(randomForest_model)) {
    results$randomForest_pred <-
      randomForest:::predict.randomForest(randomForest_model, newdata = data)
  }
  results
}