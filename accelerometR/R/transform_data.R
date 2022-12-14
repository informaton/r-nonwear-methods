if(getRversion() >= "2.15.1") { utils::globalVariables(c(".","waistrawaxis1","waistrawaxis2","waistrawaxis3","dttm","se_event","timestep",
                                                        "se_event_max", "id", "raw_state", "x_axis", "y_axis", "z_axis", "covMatrixT",
                                                        "logdetS", "logdetT", "lagged_x_axis", "lagged_y_axis", "lagged_z_axis",
                                                        "covMatrixLagged", "numeric_state", "mSm_norm1", "mSm_norm2", "mDiff", "tDiff",
                                                        "logSDiff", "theta_x", "theta_y", "theta_z", "logTDiff", "state", "covMatrix",
                                                        "L2_norm"))}
#code by wubr2000, documentation by paulkr
#loading, compiling, building by wubr2000

#' @title Performs a series of useful transformations on raw accelerometry data
#' @description Transform raw accelerometry data into features that can be used
#'    for modeling and predictions. For more information on the features created
#'    in this version, see 'details'.
#' @details The features created in this version are as follows: \cr
#'    id: a factor containing the unique identifier of each subject \cr
#'    timestep: We want this as an integer starting at 0, with each time step
#'      representing a specified interval of time, or 'epoch.' \cr
#'    x_axis, y_axis, z_axis: the acceleration data in the x, y, and z
#'      directions, respectively. \cr
#'    logdetS: the natural log of the determinant of the covariance matrix of
#'      the raw acceleration data in the x, y, and z directions for each epoch. \cr
#'    raw_state: A factor variable with levels "sleep", "wake", and "nonwear". \cr
#'    state: A factor variable obtained by smoothing raw_state to exclude 
#'      unlikely events like the subject being asleep for 3 minutes in the
#'      in the middle of two long stretches of being awake. \cr
#'    L2_norm: The euclidean norm of the acceleration data in an epoch:
#'      \code{sqrt(x_axis^2 + y_axis^2 + z_axis^2)}. \cr
#'    mSm_norm1: Where \code{m} is a vector of (x_axis, y_axis, z_axis)' and
#'      \code{S} is the covariance matrix of the raw x, y, and z accelerations,
#'      this is computed by \eqn{m' S m}. \cr
#'    mSm_norm2: Similar, but for \eqn{m' S^{-1} m}. This is often known as the
#'      Mahalanobis distance \cr
#'    mDiff: \eqn{(m_t - m_{t-1})' S_{t-1} (m_t - m_{t-1} )} \cr
#'    tDiff: \eqn{(m_t - m_{t-1} )' S_{t-1}^{-1} (m_t - m_{t-1} )} \cr
#'    logSDiff: \eqn{\mbox{logdet}S_{t-1} - \mbox{logdet}S_t}
#'    theta_x, theta_y, theta_z: \eqn{\arccos}(*_axis / L2norm). These three
#'      measure the orientation of the device relative to gravity. \cr
#'    logdetT: the natural log of the determinant of the covariance matrix
#'      between the angles theta_x, theta_y, and theta_z computed within each
#'      epoch. \cr
#'    logTDiff: \eqn{\mbox{logdet}T_{t-1} - \mbox{logdet}T_t}
#' @param individual_df, a list of dataframes containing individual user's data from read_data()
#' @param epoch, an integer specified by the user to indicate the number of seconds between each interval
#' @return A data frame containing the featurized data.
#' @importFrom magrittr "%>%"
#' @export
#' @examples 
#'    filename <- system.file("extdata", "subject100010.csv",
#'                          package="accelerometR")
#'    individual_df <- read_data(path = filename,
#'                    id = '100010',
#'                    x = 'waistrawaxis1',
#'                    y = 'waistrawaxis2',
#'                    z = 'waistrawaxis3',
#'                    datetime = 'dttm',
#'                    rawstate = 'se_event')
#'    featurized_df <- transform_data(individual_df, epoch = 30)
transform_data <- function(individual_df, epoch = 30) {
  featurized_df <- lapply(individual_df, function(subject_df) {
      #Epoch number calculation
      numRows <- dim(subject_df)[1]
      groups <- as.integer(numRows / (epoch * 40)) + 1 #40 observations per second
      epochNum <- unlist(lapply(seq(groups), function(i) rep(i, epoch * 40)))[1:numRows] - 1
      
      #Table of ID, X, Y, Z, raw_state for each epoch
      print(paste0('Creating features for subject ', unique(subject_df$id)))
      basicDataPerEpoch <- subject_df %>%
        dplyr::select(waistrawaxis1, waistrawaxis2, waistrawaxis3, dttm, se_event) %>% 
        dplyr::mutate(timestep = as.integer(epochNum)) %>%
        dplyr::group_by(timestep) %>%
        dplyr::summarize(
            x_axis = median(waistrawaxis1), 
            y_axis = median(waistrawaxis2),
            z_axis = median(waistrawaxis3),
            se_event_max = names(which.max(table(se_event)))
        ) %>%
        dplyr::mutate(
            #Converts se_event to 3 raw_states
            raw_state = ifelse(grepl("Off", se_event_max), "nonwear", ifelse(grepl("Wake", se_event_max), "wake", "sleep")),
            id = rep(unique(subject_df$id), groups)
        ) %>%
        dplyr::mutate_each(dplyr::funs(as.factor), id, raw_state) %>%
        dplyr::select(id, timestep, x_axis, y_axis, z_axis, raw_state)
      
      #Table for calculating covariance matrix AND log of the determinant of covariance matrix at each epoch
      covMatrixAndLogdetSPerEpoch <- subject_df %>%
        dplyr::select(waistrawaxis1, waistrawaxis2, waistrawaxis3) %>% 
        dplyr::mutate(timestep = as.integer(epochNum)) %>%
        dplyr::group_by(timestep) %>% 
        #Create log of determinant of covariance matrix for each timestep
        dplyr::do(covMatrix = data.frame(cov(cbind(.$waistrawaxis1,.$waistrawaxis2,.$waistrawaxis3)))) %>%
        dplyr::mutate(logdetS = log(det(matrix(unlist(covMatrix),3,3,byrow = TRUE))))
      
      #Table for calculating log of the determinant of covariance matrix for theta at each epoch
      logdetTPerEpoch <- subject_df %>%
        dplyr::select(x_axis = waistrawaxis1, y_axis = waistrawaxis2, z_axis = waistrawaxis3) %>% 
        dplyr::mutate(timestep = as.integer(epochNum),
               L2_norm = sqrt(x_axis^2 + y_axis^2 + z_axis^2),
               theta_x = acos(x_axis / L2_norm),
               theta_y = acos(y_axis / L2_norm),
               theta_z = acos(z_axis / L2_norm)) %>%
        dplyr::group_by(timestep) %>%
        dplyr::do(covMatrixT = data.frame(cov(cbind(.$theta_x,.$theta_y,.$theta_z)))) %>%
        dplyr::mutate(logdetT = log(det(matrix(unlist(covMatrixT),3,3,byrow = TRUE))))
      
      #Merge basicDataPerEpoch and covMLogDetPerEpoch and calculate remaining derivative features
      #The output of this dplyr pipeline is the final datatable for each user which is returned by the function 
      print(paste0('Preparing final featurized dataframe for subject ', unique(subject_df$id)))
      finalDF <-  merge(basicDataPerEpoch, covMatrixAndLogdetSPerEpoch, by='timestep') %>%
        merge(logdetTPerEpoch, by = 'timestep') %>%
        dplyr::mutate(
          lagged_x_axis = lag(x_axis, 1),
          lagged_y_axis = lag(y_axis, 1),
          lagged_z_axis = lag(z_axis, 1),
          L2_norm = sqrt(x_axis^2 + y_axis^2 + z_axis^2),
          covMatrixLagged = lag(covMatrix, 1),
          logSDiff = lag(logdetS, 1) - logdetS,
          theta_x = acos(x_axis / L2_norm),
          theta_y = acos(y_axis / L2_norm),
          theta_z = acos(z_axis / L2_norm),
          logTDiff = lag(logdetT, 1) - logdetT,
          numeric_state = if(length(unique(raw_state))==1) {NA} else { as.character( mhsmm::smooth.discrete(as.numeric(raw_state))$s ) }
        ) %>%
        #Need to apply rowise() first in order to perform matrix multiplications.
        dplyr::rowwise() %>%
        dplyr::mutate(
          mSm_norm1 = t(rbind(x_axis, y_axis, z_axis)) %*% 
                      matrix(unlist(covMatrix),3,3) %*% 
                      rbind(x_axis, y_axis, z_axis),
          mSm_norm2 = t(rbind(x_axis, y_axis, z_axis)) %*% 
                      solve(matrix(unlist(covMatrix),3,3)) %*% 
                      rbind(x_axis, y_axis, z_axis),
          mDiff = t(rbind(x_axis, y_axis, z_axis) - rbind(lagged_x_axis, lagged_y_axis, lagged_z_axis)) %*%
                  matrix(unlist(covMatrixLagged),3,3) %*%
                  (rbind(x_axis, y_axis, z_axis) - rbind(lagged_x_axis, lagged_y_axis, lagged_z_axis)),
          tDiff = t(rbind(x_axis, y_axis, z_axis) - rbind(lagged_x_axis, lagged_y_axis, lagged_z_axis)) %*%
                  solve(matrix(unlist(covMatrixLagged),3,3)) %*%
                  (rbind(x_axis, y_axis, z_axis) - rbind(lagged_x_axis, lagged_y_axis, lagged_z_axis))
        ) %>%
        dplyr::select(id, x_axis, y_axis, z_axis, timestep, logdetS, raw_state, numeric_state, L2_norm, mSm_norm1, 
               mSm_norm2, mDiff, tDiff, logSDiff, theta_x, theta_y, theta_z, logdetT, logTDiff) 
    
    #Convert smooth.discrete() numeric states back to readable states
    #Use that fact that with unique(), the returned results for both raw_states and numeric_states are ranked by frequency
    #Since library(mhsmm) converts string factors to numeric factors by frequency, we used the unique() function to re-convert numeric factors back to string factors.
    ranked_raw_state <- unique(finalDF$raw_state)
    ranked_numeric_state <- unique(finalDF$numeric_state)
    names(ranked_raw_state) <- ranked_numeric_state
    finalDF$state = ranked_raw_state[finalDF$numeric_state]
    
    #Return final individual featurized dataframe
    finalDF %>% dplyr::select(id, x_axis, y_axis, z_axis, timestep, raw_state, state, logdetS, L2_norm, mSm_norm1, mSm_norm2, 
                       mDiff, tDiff, logSDiff, theta_x, theta_y, theta_z, logdetT, logTDiff)
    }
  )
  
  #Combine all individual dataframes
  invisible(do.call(rbind, featurized_df))
}
