#loading, compiling, building by wubr2000
# code by wubr2000, documentation by paulkr
#' @title Read in raw accelerometry data from csv to data frame
#' @description Reads accelerometry data from a .csv file and outputs the result
#'    in a data frame. The input accelerometry data is expected to contain
#'    certain fields, namely acceleration measured in the x, y, and z
#'    directions, the time each measurement was taken, the id of the subject(s)
#'    either in a column or as a single value describing an entire file, and
#'    optionally the state of the subject (e.g. "sleep", "wake", "nonwear").
#' @param path a list of file paths specifying the locations of the files to be
#'    read. Each path within the list should be a string.
#' @param id a list of subject id's in one-to-one correspondence with the list
#'    of file paths. Each id should be a string which either denotes a single
#'    id number corresponding to the entire file, e.g. \code{"100010"}, or a
#'    column within the file in which the id information can be found, e.g.
#'    \code{"SubjectID"}.
#' @param x,y,z A string specifying the column name in which the acceleration
#'    data in the x, y, and z directions can be found.
#' @param datetime A string specifying the column name in which the timestamp
#'    data can be found.
#' @param rawstate An optional string specifying the column name in which the
#'    state data can be found. If \code{NULL}, the file is assumed not to
#'    contain state information.
#' @return A list of data frames (one per file) containing the following columns: \cr
#'    id \cr
#'    dttm \cr
#'    se_event \cr
#'    waistrawaxis1 \cr
#'    waistrawaxis2 \cr
#'    waistrawaxis3
#' @export
#' @examples 
#' filename <- system.file("extdata", "subject100010.csv",
#'                          package="accelerometR")
#' df <- read_data(path = filename,
#'                    id = '100010',
#'                    x = 'waistrawaxis1',
#'                    y = 'waistrawaxis2',
#'                    z = 'waistrawaxis3',
#'                    datetime = 'dttm',
#'                    rawstate = 'se_event')
#' str(df)
read_data <- function(path, id, x, y , z, datetime, rawstate=NULL) {
  if (is.null(id)) stop(print("No subject IDs supplied"))
  if (is.null(path)) stop(print("No filepaths supplied"))
  
  subject_df <-
    lapply(seq(path), function(count) {
      #Error message in case number of subject IDs are fewer than the number of
      #raw files entered.
      tryCatch( id[[count]],  error = function(c) {
        c$message <- paste0(c$message, ": The number of subject Ids are fewer than the number of raw files supplied.")
        stop(c)
      } )
      
      #Read in individual file
      print(paste0('Reading data for subject ', id[[count]]))
      df <- read.table(path[[count]], header = TRUE, sep= ",", fill = TRUE)
      
      #Check that the column names entered by a user are all actually found in
      #the file.
      lapply(c(x,y,z,datetime, rawstate), function(i) {
        if(!(i %in% names(df))) stop(paste0(i, " is not a column name in the file for subject ",
                                            id[[count]]))
      })
      
      #Rename the columns to standardized names
      #This ensures that all column names were entered by user - if one of these
      #columns is missing, there will be an error.
      names(df)[names(df)==x] <- 'waistrawaxis1'
      names(df)[names(df)==y] <- 'waistrawaxis2'
      names(df)[names(df)==z] <- 'waistrawaxis3'
      names(df)[names(df)==datetime] <- 'dttm'
      
      #Raw state might not exist (e.g. new users whom we want to make
      #predictions). In this case, we fill in NAs for the se_event column.
      if(is.null(rawstate)) { 
        df$se_event = "NA" 
      } else {
        names(df)[names(df)==rawstate] <- 'se_event'
      }
      
      #Fill in ID column
      df$id <- id[[count]]
      
      #Return individual's dataframe
      invisible(df)
      }
    )
}