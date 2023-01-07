library(accelerometR,lib.loc="~/Rpackages/")
library(mvtnorm,lib.loc="~/Rpackages/")
library(mhsmm,lib.loc="~/Rpackages/")
library(combinat,lib.loc="~/Rpackages/")
library(dplyr,lib.loc="~/Rpackages/")
library(magrittr,lib.loc="~/Rpackages/")

args = commandArgs(TRUE)
#load input dataset
load("/scratch/users/sahejr/dataPrep/new5hr/compressed/5hrdataset_allFeats.Rda")
#load("5hrdataset_allFeats.Rda")
#load("~/Desktop/QSU/dataPrep/5hrdataset.Rda")
#load("/scratch/users/sahejr/dataPrep/dataset3.Rda")
raw = finalDataset #assume loaded dataset is called finalDataset and save copy as raw

#convert 3 state data into 2 state data (nonwear vs wear (sleep/wake) )
raw$raw_state <- as.numeric(raw$raw_state) #nonwear is 1, wear is 2
raw$new_state <- with(raw, ifelse(raw$raw_state == 1, 1, 2))

#HANDLING 0 values--replace rows with 
zeroRows = which(is.na(raw$theta_x) | is.na(raw$theta_y) | is.na(raw$theta_z) | is.na(raw$detT))
#zeroRows = which(is.na(raw$x_axis) | is.na(raw$y_axis) | is.na(raw$z_axis) | is.na(raw$detS))
for (r in zeroRows) {
  raw[r,] =raw[r-1,]
}
#raw = raw %>% mutate_at(c("theta_x","theta_y","theta_z","detT"),funs(replace(.,is.na(.),c())))
#raw = raw %>% mutate_at(c("theta_x","theta_y","theta_z","detT"),funs(replace(.,is.na(.),0)))

raw = raw %>% mutate(logDetS = log(detS)) %>% mutate(logDetT = log(detT)) %>% select(id,timestep,x_axis,y_axis,z_axis,logDetS,theta_x,theta_y,theta_z,logDetT,new_state)
#raw = raw %>% select(id,timestep,x_axis,y_axis,z_axis,detS,new_state)
#raw = raw %>% mutate(logDetS = log(detS)) %>% select(id,timestep,x_axis,y_axis,z_axis,logDetS,new_state)

patients <- c(100010,100032,100135,100179,100266,100272,100294,100303,100325,100331,100375,100509,100521,100537,100559)

#RUN MANY SIMULATIONS OF MULTIPLE SEQUENCE TRAINING + REMAINING TEST 
numTrainingSeq = as.numeric(args[1])
numSimulations = as.numeric(args[2])
idCombos = combn(patients,numTrainingSeq,simplify=FALSE)
trainingSeqs = sample(idCombos,numSimulations)

idNums = c()
classErrVect = c()
sensVect = c()
specVect = c()
precVect = c()
npvVect = c()
classTable = 0

for (chosenIDs in trainingSeqs) {
  
  #randomly choose 3 patients to put into training set
  
  #2. train on all df's in permutation--separately
  allTrainSet = raw[raw$id %in% chosenIDs,]
  
  objList = list()
  for (id in chosenIDs) {
    obj = list(smooth.discrete(allTrainSet$new_state[allTrainSet$id==id]))
    objList = append(objList,obj) ###PROBLEM HERE?
  }
  featureSelect <- c(3:10) #x,y,z,logDet(cov matrix of triaxial)
  Mean1=aggregate(allTrainSet[,featureSelect], list(allTrainSet$new_state), mean)
  Mean1
  Cov1=cov(allTrainSet[allTrainSet$new_state==1,featureSelect])
  Cov1
  Cov2=cov(allTrainSet[allTrainSet$new_state==2,featureSelect])
  Cov2
  
  ### List of means and cov1 and 2 for each id
  
  #initial probabilies uniformly set and transmisison matrix set with high inertia
  J<-2
  initial <- rep(1/J,J)
  P <- matrix(c(
    0.995,0.005,
    0.005,0.995
  ), nrow=J)
  
  #epsilon adjustment (TOGGLE ON OFF)
  epsilon <- diag(0.00,length(featureSelect),length(featureSelect))
  
  #sets parameters for emission probability distribution
  b <- list(mu=list(c(as.numeric(Mean1[1,2:(length(featureSelect)+1)])),
                    c(as.numeric(Mean1[2,2:(length(featureSelect)+1)]))),
            sigma=list(matrix(as.numeric(Cov1+epsilon),ncol=length(featureSelect)),
                       matrix(as.numeric(Cov2+epsilon),ncol=length(featureSelect))))
  model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)
  
  # model FIX THIS COMMENT
  #300 is arbitrary
  train <- simulate(model, nsim=rep(300,length(chosenIDs)), seed=1234, rand.emis=rmvnorm.hsmm)
  train$s = c()
  train$N = c()
  for (idData in objList) {
    train$s = c(train$s,idData$s)
    train$N = c(train$N,length(idData$s))
  }
  train$x = allTrainSet[,featureSelect]
  
  h1 = hmmfit(train,model,mstep=mstep.mvnorm)
  summary(h1)
  
  #3. after that loop, grab all other ids and predict seq of hidden states for each
  testIDs = patients[!(patients %in% chosenIDs)]
  
  for (id in testIDs) {
    testSet = raw[raw$id == id,]
    #create hmmdata object (same as training) to hold test set observed state data
    test = train
    test$s = testSet$new_state #aim to predict all of this; no smoothing because true
    test$x = testSet[,featureSelect]
    test$N = dim(testSet)[1] #1 is rows 
    yhat = predict(h1,test)
    
    classError <- mean(yhat$s!=test$s)
    classTable <- table(yhat$s,test$s)
    
    classMatrix <- matrix(classTable, nrow=2, ncol=2)
    
    #corner cases for resulting table being of too few dimensions if predictions are of only 1 state
    #this is merely for ease and uniformity of output
    if ( all(yhat$s==1) ) {
      classMatrix <- matrix(c(classTable[1],0,classTable[2],0), nrow = 2, ncol = 2)  
      # dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
    } else if ( all(yhat$s==2) ) {
      classMatrix <- matrix(c(0,classTable[1],0,classTable[2]), nrow = 2, ncol = 2)
      # dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
    }
    classError <- (classMatrix[1,2]+classMatrix[2,1])/sum(classMatrix) #compare 40% made from prediction to 40% of actual state data
    sensitivity <- classMatrix[1,1] / colSums(classMatrix)[[1]] # true positives (nonwear) : total actual positives
    specificity <- classMatrix[2,2] / colSums(classMatrix)[[2]] # true negatives (wear) : total actual negatives
    precision <- classMatrix[1,1] / rowSums(classMatrix)[[1]] # true positives : total predicted positives
    npv <- classMatrix[2,2] / rowSums(classMatrix)[[2]] # true negatives : total predicted negative
    classErrVect = c(classErrVect,classError)
    sensVect = c(sensVect,sensitivity)
    specVect = c(specVect,specificity)
    precVect = c(precVect,precision)
    npvVect = c(npvVect,npv)
    idNums = c(idNums,id)
  }
}

performance = data.frame(idNums, round(classErrVect,2), round(precVect,2), round(sensVect,2), round(specVect,2), round(npvVect,2))
write.csv(file=paste0(paste0("/home/users/sahejr/5hr/HMM_multiTrainResults_5hrManyFeats_",numTrainingSeq),".csv"),x=performance)
