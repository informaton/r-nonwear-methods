# code by paulkr, documentation by paulkr
#loading, compiling, building by wubr2000

#' @title Chooses how many subjects in your data place into the training set.
#' @description A function internal to the accelerometR package designed to help
#'    split the input dataset into a training set and test set for the purposes
#'    of estimating the test classification error. It decides how many test
#'    subjects should go into the training set, and handles edge cases for when
#'    the number of subjects is very small.
#' @param num_subjects The number of unique subject id's in the accelerometry
#'    data.
#' @param training_fraction The approximate desired fraction of the total number
#'    of subjects to place in the training set. It is only approximate because
#'    the number of subjects who end up in the training set must be an integer.
#' @return An integer representing a best-attempt to put the desired fraction of
#'    subjects into the training set while still leaving at least one subject
#'    in the test set. If there is only one subject in the input data, that
#'    subject will be placed in the training set.
#' @keywords internal
choose_training_set_size <- function(num_subjects, training_fraction) {
  if (!is.numeric(training_fraction) | (training_fraction <= 0 | training_fraction > 1)) {
    warning("The training_fraction you specified was invalid. Using default value of training_fraction = 0.75.")
    training_fraction <- 0.75
  } else if (training_fraction < 0.5) {
    warning("The training_fraction you specified is smaller than recommended and may result in an inflated estimate of prediction error. Typical values for training_fraction are at least 0.5.")
  }
  
  if (num_subjects == 1) {
    warning("There is only 1 subject in your dataset, not enough to produce an estimate of test error.")
    training_size <- 1
  } else if (num_subjects < 1) {
    stop("Subject id's not found.")
  } else {
    training_size <- floor( num_subjects * training_fraction )
    if (training_size == num_subjects) {
      training_size <- training_size - 1
    }
    if (training_size == 0) {
      training_size <- 1
    }
  }
  training_size
}