# code by paulkr, documentation paulkr
#' @title Fits a variety of statistical models to accelerometry data
#' @description This is a convenience function that takes care of all of the
#'    many idiosyncracies involved in fitting models from the various
#'    statistical packages employed by the accelerometR.
#'    
#'    The current version of accelerometR only uses mhsmm and randomForest
#'    models, but future versions will implememnt more.
#' @param data A data frame of accelerometry data. It is required to be in the 
#'    format produced by the transform function in this package.
#' @param training_fraction The approximate desired fraction of the total number
#'    of subjects to place in the training set. It is only approximate because
#'    the number of subjects who end up in the training set must be an integer.
#' @param hmm,randomForest Logical. If \code{TRUE}, a model of the corresponding
#'    type will be fit to the data. If all are \code{FALSE}, then \code{build()}
#'    simply returns an empty list.
#' @param ntree A parameter passed to the
#'    \code{\link[randomForest]{randomForest}} function specifying the number of
#'    trees to grow. See \link[randomForest]{randomForest} for details.
#' @param maxnodes A parameter passed to the
#'    \code{\link[randomForest]{randomForest}} function specifying the maximum
#'    number of terminal nodes trees in the forest can have. See
#'    \link[randomForest]{randomForest} for details.
#' @seealso \code{\link[randomForest]{randomForest}},
#'    \code{\link[mhsmm]{hmmfit}}
#' @importFrom stats predict
#' @export
#' @examples 
#'    data(acclR_data)
#'    results <- build(acclR_data)
build <- function(data, training_fraction = 0.75, hmm = TRUE,
                  randomForest = TRUE, ntree = 1000, maxnodes = 10) {
  unique_ids <- unique(data$id)
  num_subjects <- length(unique_ids)
  training_set_size <- choose_training_set_size(num_subjects, training_fraction)
  test_set_exists <- (training_set_size < num_subjects)
  
  #Split the subjects randomly into training and test sets
  subsets <- c(rep("train", training_set_size),
               rep("test", num_subjects - training_set_size))
  shuffled_ids <- sample(unique_ids, size = num_subjects)
  subsets <- data.frame(subsets, shuffled_ids)
  colnames(subsets) <- c("subset", "id")
  data <- merge(data, subsets, by = "id")
  subsetted_data <- split(data, f = data$subset)
  training_data <- subsetted_data$train
  if (test_set_exists) {
    testing_data <- subsetted_data$test
  }
  
  results <- list()
  
  #hmm
  if (hmm) {
    # Build model on training data
    hmm_training_model <- build_hmm_model(training_data)
    
    # Compute predictions
    hmm_predictions <- predict_accelerometR(data, hmm_model = hmm_training_model,
                                            randomForest_model = NULL)$hmm_pred
    
    # Compute classification error tables
    results$hmm$training_class_table <-
      compute_class_tables( predicted = hmm_predictions[data$subset == "train"],
                            actual = data[data$subset == "train",
                                          colnames(data) == "state"],
                            id = data[data$subset == "train",
                                      colnames(data) == "id"])
    if (test_set_exists) {
      results$hmm$test_class_table <-
        compute_class_tables( predicted = hmm_predictions[data$subset == "test"],
                              actual = data[data$subset == "test",
                                            colnames(data) == "state"],
                              id = data[data$subset == "test",
                                        colnames(data) == "id"])
    }
    # Build model on full data
    if (test_set_exists) {
      results$hmm$model <- build_hmm_model(data)
    } else {
      results$hmm$model <- hmm_training_model
    }
  }
  
  if (randomForest) {
    # Build model on training data
    rForest_training_model <-
      randomForest::randomForest(state ~ logdetS + logSDiff + theta_x + theta_y
                                 + theta_z + mSm_norm2 + mDiff + tDiff,
                                 data = training_data, na.action = na.omit,
                                 keep.forest = TRUE, ntree = ntree, 
                                 maxnodes = maxnodes)
    
    # Compute predictions
    rForest_predictions <-
      randomForest:::predict.randomForest(rForest_training_model, newdata = data)
    
    # Compute classification error tables
    results$randomForest$training_class_table <-
      compute_class_tables(predicted = rForest_predictions[data$subset == "train"],
                           actual = data[data$subset == "train",
                                         colnames(data) == "state"],
                           id = data[data$subset == "train",
                                     colnames(data) == "id"])
    if (test_set_exists) {
      results$randomForest$test_class_table <-
        compute_class_tables( predicted = rForest_predictions[data$subset == "test"],
                              actual = data[data$subset == "test",
                                            colnames(data) == "state"],
                              id = data[data$subset == "test",
                                        colnames(data) == "id"])
    }
    
    # Build model on full data
    if (test_set_exists) {
      results$randomForest$model <-
        randomForest::randomForest(state ~ logdetS + logSDiff + theta_x + theta_y + theta_z +
                       mSm_norm2 + mDiff + tDiff, data = data,
                     na.action = na.omit, keep.forest = TRUE, ntree = ntree,
                     maxnodes = maxnodes)
    } else {
      results$randomForest$model <- rForest_training_model
    }
  }
  results
}