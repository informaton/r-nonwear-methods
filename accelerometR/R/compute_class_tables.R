# code by paulkr, documentation by paulkr
#loading, compiling, building by wubr2000

#' @title Compute classification tables
#' @description Creates a list of classification tables given predicted values,
#'    actual values, and subject ids.
#' @param predicted A vector of predicted values.
#' @param actual A vector of actual values.
#' @param id a factor of subject id's.
#' @return A list of classification tables. The structure of the list is as
#'    follows:
#'    
#'    \code{..$frequency} \cr
#'    \code{.. ..$by_actual} \cr
#'    \code{.. ..$by_actual_and_id} \cr
#'    \code{..$rate} \cr
#'    \code{.. ..$by_actual} \cr
#'    \code{.. ..$by_actual_and_id}
#'    
#'    The tables in \code{..$frequency} give the actual number of observations
#'    in each cell. The tables in \code{..$rate} give the row-wise fraction of
#'    observations in each cell. The tables under \code{.. ..$by_actual} are
#'    2-dimensional tables, with predicted classes on the first dimension and
#'    actual classes on the second dimension. The tables under
#'    \code{.. ..$by_actual_and_id} are 3-dimensional tables, with subject id on
#'    the first dimension, predicted classes on the second dimension, and actual
#'    classes on the third dimension. The \code{.. ..$by_actual_and_id} tables
#'    are there so that the user can explore whether the various models perform
#'    better on some subjects than on others.
#' @export
#' @examples 
#'    data(acclR_data)
#'    predictions <- predict_accelerometR(acclR_data)$randomForest_pred
#'    class_tables <- compute_class_tables(predictions,
#'    acclR_data$state, acclR_data$id)
compute_class_tables <- function(predicted, actual, id) {
  x <- as.data.frame(matrix(0, nrow = length(id), ncol = 3))
  colnames(x) <- c("id", "predicted", "actual")
  x$id <- id
  x$predicted <- predicted
  x$actual <- actual
  
  class_freqs <- table(x[, colnames(x) %in% c("actual", "predicted")])
  class_rates <- class_freqs / rowSums(class_freqs)
  class_freqs_by_id <- table(x)
  class_rates_by_id <- table(x)
  for (i in 1:dim(class_rates_by_id)[1]) {
    for (j in 1:dim(class_rates_by_id)[2]) {
      class_rates_by_id[i, , j] <- class_rates_by_id[i, , j] /
        sum(class_rates_by_id[i, , j])
    }
  }
  
  list(frequency = list(by_actual = class_freqs,
                        by_actual_and_id = class_freqs_by_id),
       rate = list(by_actual = class_rates,
                   by_actual_and_id = class_rates_by_id))
}