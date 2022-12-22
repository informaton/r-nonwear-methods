# save vars for comparison
# get rid of trainSet if possible
# REMINDER: more efficient patient ID switch
# FUTURE NOTE: Could have parameter for cutoff point
# FEATURE SELECT HAS BEEN ALTERED
library(accelerometR)
library(mhsmm)
library(ggplot2)

setDF <- function(subjectID) {
  if ( subjectID == "100010" ) {
    intervals <- c(1:60,120:1609,1670:1730)
    df <<- acclR_data[acclR_data$id == "100010" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100032" ) {
    intervals <- c(1:60,120:1532,1595:1655)
    df <<- acclR_data[acclR_data$id == "100032" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100135" ) {
    intervals <- c(1:60,120:1464,1525:1580)
    df <<- acclR_data[acclR_data$id == "100135" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100179" ) {
    intervals <- c(1:60,120:1822,1863:1942)
    df <<- acclR_data[acclR_data$id == "100179" & acclR_data$timestep %in% intervals, ]
  } 
  else if ( subjectID == "100266" ) {
    intervals <- c(1:60,120:1445,1506:1565)
    df <<- acclR_data[acclR_data$id == "100266" & acclR_data$timestep %in% intervals, ]
  } 
  else if ( subjectID == "100272" ) {
    intervals <- c(1:60,120:1546,1605:1662)
    df <<- acclR_data[acclR_data$id == "100272" & acclR_data$timestep %in% intervals, ]
  } 
  else if (subjectID == "100294" ) {
    intervals <- c(1:60,120:1452,1511:1572)
    df <<- acclR_data[acclR_data$id == "100294" & acclR_data$timestep %in% intervals, ]
  } 
  else if ( subjectID == "100303" ) {
    intervals <- c(1:60,120:1366,1427:1487)
    df <<- acclR_data[acclR_data$id == "100303" & acclR_data$timestep %in% intervals, ]
  } 
  else if ( subjectID == "100325" ) {
    intervals <- c(1:60,120:1518,1579:1639)
    df <<- acclR_data[acclR_data$id == "100325" & acclR_data$timestep %in% intervals, ]
  } 
  else if ( subjectID == "100331" ) {
    intervals <- c(1:60,120:1642,1703:1762)
    df <<- acclR_data[acclR_data$id == "100331" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100375" ) {
    intervals <- c(1:60,120:1529,1590:1650)
    df <<- acclR_data[acclR_data$id == "100375" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100509" ) {
    intervals <- c(1:60,120:1255,1316:1375)
    df <<- acclR_data[acclR_data$id == "100509" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100521" ) {
    intervals <- c(1:60,120:1264,1324:1384)
    df <<- acclR_data[acclR_data$id == "100521" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100537" ) {
    intervals <- c(1:60,120:1524,1584:1645)
    df <<- acclR_data[acclR_data$id == "100537" & acclR_data$timestep %in% intervals, ]
  }
  else if ( subjectID == "100559" ) {
    intervals <<- c(1:60,120:1527,1588:1648)
    df <<- acclR_data[acclR_data$id == "100559" & acclR_data$timestep %in% intervals, ]
  }
}

#dataname - acclR_data

#convert 3 state data into 2 state data (nonwear vs wear (sleep/wake) )
acclR_data$raw_state <- as.numeric(acclR_data$raw_state)
acclR_data$new_state <- with(acclR_data, ifelse(acclR_data$raw_state == 1, 1, 2))

#separate data by subject Id and corresponding non-noise time intervals
#df=acclR_data[acclR_data$id == 100559 & acclR_data$timestep %in% intervals,]
subject <- "100521"
intervals <- NULL
df <- NULL
setDF(subject)



#intervals <- c(0:61,120:1527,1588:1648)
df <- acclR_data[acclR_data$id == "100521", ]
#df=acclR_data[acclR_data$id == 100559 & acclR_data$timestep %in% intervals,]

obj <- smooth.discrete(df$new_state)
dim(df)

#index delineating the training set as the first 60% of the data
#remaining 40% of data forms the test set to compare against
partitionIndex <- floor(0.60*dim(df))[1]

#obtain mean and cov of x, y, z counts of training set for model building
featureSelect <- c(3:6) #x,y,z,logDet(cov matrix of triaxial),norm of triaxial
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
model
train <- simulate(model, nsim=300, seed=1234, rand.emis=rmvnorm.hsmm)
train$s = obj$s[1:partitionIndex] #first 60% of true states
train$x = df[1:partitionIndex,featureSelect] #first 60% of observed states
train$N = partitionIndex

h1 = hmmfit(train,model,mstep=mstep.mvnorm) #train on first 60% of data
summary(h1)

#overwrite train values to now hold last 40% of data to compare against
train$s = obj$s[-(1:partitionIndex)] #last 40% of true states
train$x = df[-(1:partitionIndex),featureSelect] #last 40% of observed states
train$N = dim(df)[1]-partitionIndex

yhat = predict(h1,train) #train should hold last 40% of data here
mean(yhat$s!=obj$s[-(1:partitionIndex)]) #compare 40% made from prediction to 40% of actual state data
table(yhat$s,obj$s[-(1:partitionIndex)])
#plot(table(yhat$s,obj135$s[-(1:partitionIndex)]))

#make classification table:
#compute_class_tables(yhat$s, obj$s[-(1:partitionIndex)],rep("100559",length(yhat$s)))
















#Repeat for thetas
#obtain mean and cov of x, y, z counts of training set for model building
trainSet <- df[1:partitionIndex,]
featureSelect <- c(14:17)
Mean1=aggregate(trainSet[,featureSelect], list(trainSet$new_state), mean)
Mean1
Cov1=cov(trainSet[trainSet$new_state==1,featureSelect])
Cov1
Cov2=cov(trainSet[trainSet$new_state==2,featureSelect])
Cov2

#initial probabilies uniformly set and transmisison matrix set with high inertia
J<-2
initial <- rep(1/J,J)
P <- matrix(c(
  0.995,0.005,
  0.005,0.995
), nrow=J)

#sets parameters for emission probability distribution
b <- list(mu=list(c(as.numeric(Mean1[1,2:5])),
                  c(as.numeric(Mean1[2,2:5]))),
          sigma=list(matrix(as.numeric(Cov1),ncol=4),
                     matrix(as.numeric(Cov2),ncol=4)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
model
train <- simulate(model, nsim=300, seed=1234, rand.emis=rmvnorm.hsmm)
train$s = obj$s[1:partitionIndex] #first 60% of true states
train$x = df[1:partitionIndex,featureSelect] #first 60% of observed states
train$N = partitionIndex

h1 = hmmfit(train,model,mstep=mstep.mvnorm) #train on first 60% of data
summary(h1)

#overwrite train values to now hold last 40% of data to compare against
train$s = obj$s[-(1:partitionIndex)] #last 40% of true states
train$x = df[-(1:partitionIndex),featureSelect] #last 40% of observed states
train$N = dim(df)[1]-partitionIndex
  
yhat = predict(h1,train) #train should hold last 40% of data here
mean(yhat$s!=obj$s[-(1:partitionIndex)]) #compare 40% made from prediction to 40% of actual state data
table(yhat$s,obj$s[-(1:partitionIndex)])
#plot(table(yhat$s,obj135$s[-(1:partitionIndex)]))









#without partition, to validate script:
acclR_data$raw_state <- as.numeric(acclR_data$raw_state)
acclR_data$new_state <- with(acclR_data, ifelse(acclR_data$raw_state == 1, 1, 2))

# df <- acclR_data[acclR_data$id == "100010", ]

subject <- "100179"
intervals <- NULL
df <- NULL
setDF(subject)

obj <- smooth.discrete(df$new_state)
dim(df)

#index delineating the training set as the first 60% of the data
#remaining 40% of data forms the test set to compare against
partitionIndex <- floor(1*dim(df))[1]

#obtain mean and cov of x, y, z counts of training set for model building
featureSelect <- c(3:6)
trainSet <- df[1:partitionIndex,]
Mean1=aggregate(trainSet[,featureSelect], list(trainSet$new_state), mean)
Mean1
Cov1=cov(trainSet[trainSet$new_state==1,featureSelect])
Cov1
Cov2=cov(trainSet[trainSet$new_state==2,featureSelect])
Cov2

#initial probabilies uniformly set and transmisison matrix set with high inertia
J<-2
initial <- rep(1/J,J)
P <- matrix(c(
  0.995,0.005,
  0.005,0.995
), nrow=J)

#sets parameters for emission probability distribution
b <- list(mu=list(c(as.numeric(Mean1[1,2:(length(featureSelect)+1)])),
                  c(as.numeric(Mean1[2,2:(length(featureSelect)+1)]))),
          sigma=list(matrix(as.numeric(Cov1),ncol=length(featureSelect)),
                     matrix(as.numeric(Cov2),ncol=length(featureSelect))))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
model
train <- simulate(model, nsim=300, seed=1234, rand.emis=rmvnorm.hsmm)
train$s = obj$s[1:partitionIndex] 
train$x = df[1:partitionIndex,featureSelect]
train$N = partitionIndex

h1 = hmmfit(train,model,mstep=mstep.mvnorm)
summary(h1)

#"overwrite": will make test set same as training set
train$s = obj$s[(1:partitionIndex)]
train$x = df[(1:partitionIndex),featureSelect] 
train$N = dim(df)[1]

yhat = predict(h1,train)
classError <- mean(yhat$s!=obj$s[(1:partitionIndex)]) #compare 40% made from prediction to 40% of actual state data
classTable <- table(yhat$s,obj$s[(1:partitionIndex)])
sensitivity <- classTable[1,1] / colSums(classTable)[1] # true positives (nonwear) : total actual positives
specificity <- classTable[2,2] / colSums(classTable)[2] # true negatives (wear) : total actual negatives
precision <- classTable[1,1] / rowSums(classTable)[1] # true positives : total predicted positives
#precision <- colSums(classTable)[1] / rowSums(classTable)[1] # total actual positives : total predicted positives

# Print output per subject; can send to file of all output
cat("Subject:", subject)
cat("Classification Table:"); classTable
cat("Classification Error:", round(classError,2))
cat("Precision:", round(precision,2))
cat("Sensitivity:", round(sensitivity,2))
cat("Specificity:", round(specificity,2))

#plot(table(yhat$s,obj135$s[-(1:partitionIndex)]))
#make classification table:
#compute_class_tables(yhat$s, obj$s[(1:partitionIndex)],rep("100010",length(yhat$s)))

