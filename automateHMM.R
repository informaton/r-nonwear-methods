library(accelerometR)
library(mhsmm)

saveResultsToCsv = FALSE
# saveResultsToCsv = TRUE

resultsPath = "results"
resultsFilename = "hmmPerformance.csv"

setDF <- function(subjectID) {
  if ( subjectID == "100010" ) {
    intervals <- c(1:60,120:1609,1670:1730)
    return(acclR_data[acclR_data$id == "100010" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100032" ) {
    intervals <- c(1:60,120:1532,1595:1655)
    return(acclR_data[acclR_data$id == "100032" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100135" ) {
    intervals <- c(1:60,120:1464,1525:1580)
    return(acclR_data[acclR_data$id == "100135" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100179" ) {
    intervals <- c(1:60,120:1822,1863:1942)
    return(acclR_data[acclR_data$id == "100179" & acclR_data$timestep %in% intervals, ])
  } 
  else if ( subjectID == "100266" ) {
    intervals <- c(1:60,120:1445,1506:1565)
    return(acclR_data[acclR_data$id == "100266" & acclR_data$timestep %in% intervals, ])
  } 
  else if ( subjectID == "100272" ) {
    intervals <- c(1:60,120:1546,1605:1662)
    return(acclR_data[acclR_data$id == "100272" & acclR_data$timestep %in% intervals, ])
  } 
  else if (subjectID == "100294" ) {
    intervals <- c(1:60,120:1452,1511:1572)
    return(acclR_data[acclR_data$id == "100294" & acclR_data$timestep %in% intervals, ])
  } 
  else if ( subjectID == "100303" ) {
    intervals <- c(1:60,120:1366,1427:1487)
    return(acclR_data[acclR_data$id == "100303" & acclR_data$timestep %in% intervals, ])
  } 
  else if ( subjectID == "100325" ) {
    intervals <- c(1:60,120:1518,1579:1639)
    return(acclR_data[acclR_data$id == "100325" & acclR_data$timestep %in% intervals, ])
  } 
  else if ( subjectID == "100331" ) {
    intervals <- c(1:60,120:1642,1703:1762)
    return(acclR_data[acclR_data$id == "100331" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100375" ) {
    intervals <- c(1:60,120:1529,1590:1650)
    return(acclR_data[acclR_data$id == "100375" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100509" ) {
    intervals <- c(1:60,120:1255,1316:1375)
    return(acclR_data[acclR_data$id == "100509" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100521" ) {
    intervals <- c(1:60,120:1264,1324:1384)
    return(acclR_data[acclR_data$id == "100521" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100537" ) {
    intervals <- c(1:60,120:1524,1584:1645)
    return(acclR_data[acclR_data$id == "100537" & acclR_data$timestep %in% intervals, ])
  }
  else if ( subjectID == "100559" ) {
    intervals <<- c(1:60,120:1527,1588:1648)
    return(acclR_data[acclR_data$id == "100559" & acclR_data$timestep %in% intervals, ])
  }
}

HMMPart <- function(id, partPercent, noiseRemoval) {

  subject <- id
  
  if ( noiseRemoval ) {
    intervals <- NULL; df <- NULL; df <- setDF(subject)
  } else {
    df <- acclR_data[acclR_data$id == subject, ]  
  }
  
  obj <- smooth.discrete(df$new_state)
  dim(df)
  
  #index delineating the training set as the first partPercent% of the data
  #remaining 40% of data forms the test set to compare against
  partitionIndex <- floor((partPercent/100)*dim(df))[1]
  
  #obtain mean and cov of x, y, z counts of training set for model building
  featureSelect <- c(3:6) #x,y,z,logDet(cov matrix of triaxial)
  trainSet <- df[1:partitionIndex,]
  Mean1=aggregate(trainSet[,featureSelect], list(trainSet$new_state), mean)
  Mean1
  Cov1=cov(trainSet[trainSet$new_state==1,featureSelect])
  Cov1
  Cov2=cov(trainSet[trainSet$new_state==2,featureSelect])
  Cov2
  
  #initial probabilities uniformly set and transmission matrix set with high inertia
  J<-2
  initial <- rep(1/J,J)
  P <- matrix(c(
    0.995,0.005,
    0.005,0.995
  ), nrow=J)
  
  epsilon <- diag(0.05,4,4)
  #sets parameters for emission probability distribution
  b <- list(mu=list(c(as.numeric(Mean1[1,2:(length(featureSelect)+1)])),
                    c(as.numeric(Mean1[2,2:(length(featureSelect)+1)]))),
            sigma=list(matrix(as.numeric(Cov1+epsilon),ncol=length(featureSelect)),
                       matrix(as.numeric(Cov2+epsilon),ncol=length(featureSelect))))
  model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
  # model
  train <- simulate(model, nsim=300, seed=1234, rand.emis=rmvnorm.hsmm)
  train$s = obj$s[1:partitionIndex] #first 60% of true states
  train$x = df[1:partitionIndex,featureSelect] #first 60% of observed states
  train$N = partitionIndex
  
  #train: refine parameters based on first 60% of data
  h1 = hmmfit(train,model,mstep=mstep.mvnorm)
  # summary(h1)
  
  #overwrite train values to now hold last 40% of data to compare against
  train$s = obj$s[-(1:partitionIndex)] #last 40% of true states
  train$x = df[-(1:partitionIndex),featureSelect] #last 40% of observed states
  train$N = dim(df)[1]-partitionIndex
  
  yhat = predict(h1,train) #train should hold last 40% of data here
  classError <- mean(yhat$s!=obj$s[-(1:partitionIndex)]) #compare 40% made from prediction to 40% of actual state data
  classTable <- table(yhat$s,obj$s[-(1:partitionIndex)])
  
  classMatrix <- matrix(classTable, nrow=2, ncol=2)
  if ( all(yhat$s==1) ) {
    classMatrix <- matrix(c(classTable[1],0,classTable[2],0), nrow = 2, ncol = 2)  
    # dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
  }
  else if ( all(yhat$s==2) ) {
    classMatrix <- matrix(c(0,classTable[1],0,classTable[2]), nrow = 2, ncol = 2)
    # dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
  }
  classError <- (classMatrix[1,2]+classMatrix[2,1])/sum(classMatrix) #compare 40% made from prediction to 40% of actual state data
  sensitivity <- classMatrix[1,1] / colSums(classMatrix)[[1]] # true positives (nonwear) : total actual positives
  specificity <- classMatrix[2,2] / colSums(classMatrix)[[2]] # true negatives (wear) : total actual negatives
  precision <- classMatrix[1,1] / rowSums(classMatrix)[[1]] # true positives : total predicted positives
  npv <- classMatrix[2,2] / rowSums(classMatrix)[[2]] # true negatives : total predicted negative
    
  cat("Subject:", id, "\n")
  cat("Classification Table:\n"); print(classMatrix); cat("\n")
  cat("Classification Error:", round(classError,2),"\n")
  cat("Precision:", round(precision,2),"\n")
  cat("Sensitivity:", round(sensitivity,2),"\n")
  cat("Specificity:", round(specificity,2),"\n")
  cat("NPV:", round(npv,2),"\n", "\n")
  invisible(data.frame(subject, round(classError,2), round(precision,2), round(sensitivity,2), round(specificity,2), round(npv,2), noiseRemoval))
}



#convert 3 state data into 2 state data (nonwear vs wear (sleep/wake) )
acclR_data$raw_state <- as.numeric(acclR_data$raw_state)
acclR_data$new_state <- with(acclR_data, ifelse(acclR_data$raw_state == 1, 1, 2))


# patients <- c(100010,100032,100135,100179,100266,100272,100294,100303,100325,100331,100375,100509,100521,100537,100559)
patients <- c(100010,100303,100032,100325,100331,100375,100135,100179,100509,100521,100266,100537,100272,100559,100294)
performance <- data.frame()
tempDF <- data.frame()
partPercent <- 50

for (i in patients) {
  tryCatch( {
    performance <<- rbind(performance,HMMPart(i,partPercent,TRUE))
  },error = function(e) {
    cat("Error struck, allowing noisy intervals...\n")
    cat("on index: ")
    cat(i)
    cat("\n")
    performance <<- rbind(performance,HMMPart(i,partPercent,FALSE))
    }
  )
}

if(saveResultsToCsv){
  csvFile = file.path(resultsPath,resultsFilename)
  write.csv(file=csvFile,x=performance)
  print(paste0("Results saved to ", csvFile))
}
