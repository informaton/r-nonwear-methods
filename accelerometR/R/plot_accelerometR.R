# code by manojs, documentation by paulkr
#loading, compiling, building by wubr2000

#' @title Plot predited vs. actual states for easy comparison
#' @description Creates a complex plot comparing one set of state predictions to
#'    the actual states. Care is taken to highlight the points for which the
#'    predictioned states \emph{differ} from the actual states.
#' @param predicted A factor vector giving the sequence of state predictions
#'    over time.
#' @param actual A factor vector giving the sequence of actual states over time.
#' @param main A string giving the title of the resulting plot.
#' @param timelabel A string giving the x-axis label of the resulting plot.
#' @export
#' @examples 
#'    data(acclR_data)
#'    subject1data <- acclR_data[acclR_data$id == 100010, ]
#'    predicted <-
#'    predict_accelerometR(subject1data, hmm = NULL)$randomForest_pred
#'    actual <- subject1data$state
#'    plot_accelerometR(predicted, actual,
#'    main = "Actual vs. predicted state, Subject 100010")
plot_accelerometR <- function (predicted, actual,
                               main = "Actual vs. predicted state",
                               timelabel = "Timesteps (30 second intervals)") {
	# Find prediction errors
	time <- 1:length(actual)
	errors <- data.frame(time, actual, predicted)
	errors <- errors[actual != predicted, ]
	
	# ploting the points where the prediction was wrong
	plot(x = NA, y = NA, ylim = c(-0.5, 3.3),
	     xlim = c(-length(time)/5, length(time)*5/4),
	     main = main, xlab = timelabel,
	     ylab = "", yaxt = 'n', xaxt = 'n', bty = 'n')
	axis(side = 1, xlim = c(1, length(time)))
	rect(xleft = 1, xright = length(time), ytop = 3.3, ybottom = 0.7, col = "grey",
	     border = NA)
	
	points(x = errors$time, y = errors$actual, pch = 18, col = "black")
	points(x = errors$time, y = errors$predicted, pch = 18, col = "red")
	
	# plotting predicted and actual states
	mhsmm::addStates(as.numeric(actual), ybot = -0.5, ytop = -0.1)
	mhsmm::addStates(as.numeric(predicted), ybot = 0, ytop = 0.4)
	
	# adding proper labels
	text(x = -length(time)/20, y = 1:3, labels = levels(actual), cex = 0.8,
	     adj = c(1, NA))
	text(x = -length(time)/20, y = c(-0.3, 0.2),
	     labels = c("actual", "predicted"), cex = 0.8, adj = c(1, NA))
	text(x = length(time)*31/30, y = 2.5, labels = "Prediction",
	     adj = c(0, NA))
	text(x = length(time)*31/30, y = 2.2, labels = "Errors",
	     adj = c(0, NA))
	text(x = length(time)*31/30, y = 0.6, labels = "States", adj = c(0, NA))
	legend(length(time)*41/40, 0.4, legend = levels(actual), cex = 0.8,
	       pch = 15, col = c("#66C2A5", "#FC8D62", "#8DA0CB"))
	legend(length(time)*41/40, y = 2, legend = c("actual", "predicted"),
	       cex = 0.8, pch = 18, col = c("black", "red"))
}