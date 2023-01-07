#for only 15 subj
#partition

library(mclust,lib.loc="~/Rpackages/")
library(accelerometR,lib.loc="~/Rpackages/")
library(dplyr,lib.loc="~/Rpackages/")
library(magrittr,lib.loc="~/Rpackages/")
#dataname - acclR_data

args = commandArgs(TRUE)

fileLabel = as.numeric(args[1])

#load("/scratch/users/sahejr/dataPrep/new5hr/compressed/5hrdataset.Rda")
load("/scratch/users/sahejr/dataPrep/new5hr/compressed/5hrdataset_allFeats.Rda")
#load("/scratch/users/sahejr/dataPrep/5hrSet/uncompressed/uncompressed5hr.Rda")
raw <- finalDataset

raw$raw_state <- as.numeric(raw$raw_state)
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

generateNoiseless <- function(DF,id){
  # get slice of dataset for this patient
  patientDF <- DF[DF$id==id,]
  #find beginning and end of wear
  beginW <- which(patientDF$raw_state != "1")[1] 
  endW <- tail(which(patientDF$raw_state != "1"),n=1)
  
  #get indices 60 nonwear, 30sec intervals flanking wear
  beginNoise <- beginW - 60
  endNoise <- endW + 60
  
  #timestep starts at 0 while rows start at 1; adjusts for that; first interval at t=1 accr to previous code
  beginW <- beginW-1
  endW <- endW-1
  beginNoise <- beginNoise
  endNoise <- endNoise
  #keep all but those flanking nonwear intervals and return
  return( patientDF[ patientDF$timestep %in% c(1:(beginNoise-1),beginW:endW,endNoise:dim(patientDF)[1]), ] )
}

df_combine <- raw
dim(df_combine)

head(df_combine)

#Choose random subset of data as training set
nonwearIndices = which(df_combine$new_state==1)
wearIndices = which(df_combine$new_state==2)
numExamples = min(length(nonwearIndices),length(wearIndices))
trainingNonwear = sample( nonwearIndices, floor(0.80*numExamples), replace=FALSE)
trainingWear = sample( wearIndices, floor(0.80*numExamples), replace=FALSE)
training = c(trainingNonwear,trainingWear)

# nonwearData = df_combine[df_combine$new_state == 1,]
# wearData = df_combine[df_combine$new_state == 2,]
# numExamples = min(dim(nonwearData)[1],dim(wearData)[1])
# 
# trainingNonwear = df_combine[sample.int(dim(nonwearData)[1], floor(0.80*numExamples), replace=FALSE),]
# trainingWear = df_combine[sample.int( dim(wearData)[1], floor(0.80*numExamples), replace=FALSE),]
# 
# training = rbind(trainingWear,trainingNonwear)

featureSelect = c(3:6)

#only keep the median_x, y, and z axis and logarithm of determinant of variance covariance matrix
dataMclust <- Mclust(df_combine[training,featureSelect])

#summary(dataMclust,parameters=TRUE)
#summary(dataMclust,classification=TRUE)

#the following provides the classification into 9 clusters.
classTable <- table(dataMclust$classification,df_combine$new_state[training])
classTable
# dataMclust$classification1[training] <- with(dataMclust, ifelse(dataMclust$classification[training] == 9, 1, 2))
# table(dataMclust$classification1[training],df_combine$new_state[training],df_combine$id[training])
# table(dataMclust$classification,df_combine$new_state[training],df_combine$id[training])

clustAssign <- c( rep.int(1,nrow(classTable) ) )
for (i in 1:nrow(classTable) ) {
  if ( classTable[i,2] >= classTable[i,1] ) { clustAssign[i] = 2 }
}
cat("cluster assignment:", clustAssign, "\n")
clust1 <- which( clustAssign == 1)

#USE MODEL FROM TRAINING SET TO PREDICT CLASSIFICATION OF REMAINING 40% OF DATA
predict40 <- predict(dataMclust, df_combine[-(training),featureSelect])
predict40$classification1 <- with(predict40, ifelse(predict40$classification %in% clust1, 1, 2))
classSummary <- table(predict40$classification1,df_combine$new_state[-training],df_combine$id[-training])

table(predict40$classification,df_combine$new_state[-(training)],df_combine$id[-(training)])

idNums = c()
classErrVect = c()
sensVect = c()
specVect = c()
precVect = c()
npvVect = c()

for (id in 1:dim(classSummary)[3]) {
  classTablePerID <- classSummary[,,id]
  classMatrix <- matrix(classTablePerID, nrow=2, ncol=2)
  dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
  # for case that all clusters are assigned to one state (error causing otherwise as table removes rows)
  if ( is.null( dim(classTablePerID) ) ) {
    if ( all(predict40$classification1==1) ) { # if all 1's predicted
      classMatrix <- matrix(c(classTablePerID[1],0,classTablePerID[2],0), nrow = 2, ncol = 2)
      dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
    }
    else if ( all(predict40$classification1==2) ) { # if all 2's predicted
      classMatrix <- matrix(c(0,classTablePerID[1],0,classTablePerID[2]), nrow = 2, ncol = 2)
      dimnames(classMatrix) = list( c("predictedNW", "predictedW"), c("trueNW", "trueW"))
    }
  }
  classError <- (classMatrix[1,2]+classMatrix[2,1])/sum(classMatrix) #compare 40% made from prediction to 40% of actual state data
  sensitivity <- classMatrix[1,1] / colSums(classMatrix)[1] # true positives (nonwear) : total actual positives
  specificity <- classMatrix[2,2] / colSums(classMatrix)[2] # true negatives (wear) : total actual negatives
  precision <- classMatrix[1,1] / rowSums(classMatrix)[1] # true positives : total predicted positives
  npv <- classMatrix[2,2] / rowSums(classMatrix)[2]
  # sensitivity <- classSummary[1,1,id] / colSums(classSummary[,,id])[1] # true positives (nonwear) : total actual positives
  # specificity <- classSummary[2,2,id] / colSums(classSummary[,,id])[2] # true negatives (wear) : total actual negatives
  # precision <- classSummary[1,1,id] / rowSums(classSummary[,,id])[1] # true positives : total predicted positives
  # print("Subject:"); print( levels(df_combine$id)[id] )
  # print("Classification Table:"); print.table(classTablePerID)
  # print("Classification Error:"); print( round(classError,2) )
  # print("Precision:"); print( round(precision,2) )
  # print("Sensitivity:"); print( round(precision,2) )
  # print("Specificity:"); print( round(precision,2) )
  classErrVect = c(classErrVect,classError)
  sensVect = c(sensVect,sensitivity)
  specVect = c(specVect,specificity)
  precVect = c(precVect,precision)
  npvVect = c(npvVect,npv)
  idNums = c(idNums,id)
  
  cat("Subject:", levels(df_combine$id)[id], "\n")
  cat("Classification Table:\n"); print(classMatrix); cat("\n")
  cat("Classification Error:", round(classError,2),"\n")
  cat("Precision:", round(precision,2),"\n")
  cat("Sensitivity:", round(sensitivity,2),"\n")
  cat("Specificity:", round(specificity,2),"\n", "\n")
}

performance = data.frame(idNums, round(classErrVect,2), round(precVect,2), round(sensVect,2), round(specVect,2), round(npvVect,2))
write.csv(file=paste("/home/users/sahejr/5hr/arrayScripts/GMM80_balanced_",fileLabel,".csv",sep=""),x=performance)
