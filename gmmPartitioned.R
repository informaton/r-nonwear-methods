# GMM partition for 15 subjects

library(ggplot2)
library(mclust)
library(accelerometR)  
# dataname - acclR_data

acclR_data$raw_state <- as.numeric(acclR_data$raw_state)
acclR_data$new_state <- with(acclR_data, ifelse(acclR_data$raw_state == 1, 1, 2))

#################################Removing subjects with noisy nonwear
s1 <- c(1:60,120:1609,1670:1730)
df1 <- acclR_data[acclR_data$id == "100010" & acclR_data$timestep %in% s1, ]

s2 <- c(1:60,120:1532,1595:1655)
df2 <- acclR_data[acclR_data$id == "100032" & acclR_data$timestep %in% s2, ]

s3 <- c(1:60,120:1464,1525:1580)
df3 <- acclR_data[acclR_data$id == "100135" & acclR_data$timestep %in% s3, ]

s4 <- c(1:60,120:1822,1863:1942)
df4 <- acclR_data[acclR_data$id == "100179" & acclR_data$timestep %in% s4, ]

s5 <- c(1:60,120:1445,1506:1565)
df5 <- acclR_data[acclR_data$id == "100266" & acclR_data$timestep %in% s5, ]

s6 <- c(1:60,120:1546,1605:1662)
df6 <- acclR_data[acclR_data$id == "100272" & acclR_data$timestep %in% s6, ]

s7 <- c(1:60,120:1452,1511:1572)
df7 <- acclR_data[acclR_data$id == "100294" & acclR_data$timestep %in% s7, ]

s8 <- c(1:60,120:1366,1427:1487)
df8 <- acclR_data[acclR_data$id == "100303" & acclR_data$timestep %in% s8, ]

s9 <- c(1:60,120:1518,1579:1639)
df9 <- acclR_data[acclR_data$id == "100325" & acclR_data$timestep %in% s9, ]

s10 <- c(1:60,120:1642,1703:1762)
df10 <- acclR_data[acclR_data$id == "100331" & acclR_data$timestep %in% s10, ]

s11 <- c(1:60,120:1529,1590:1650)
df11 <- acclR_data[acclR_data$id == "100375" & acclR_data$timestep %in% s11, ]

s12 <- c(1:60,120:1255,1316:1375)
df12 <- acclR_data[acclR_data$id == "100509" & acclR_data$timestep %in% s12, ]

s13 <- c(1:60,120:1264,1324:1384)
df13 <- acclR_data[acclR_data$id == "100521" & acclR_data$timestep %in% s13, ]

s14 <- c(1:60,120:1524,1584:1645)
df14 <- acclR_data[acclR_data$id == "100537" & acclR_data$timestep %in% s14, ]

s15 <- c(1:60,120:1527,1588:1648)
df15 <- acclR_data[acclR_data$id == "100559" & acclR_data$timestep %in% s15, ]

#not using 16th subject
#s16 <- c(1:60,120:1572,1633:1693)
#df16 <- acclR_data[acclR_data$id == "100602" & acclR_data$timestep %in% s16, ]

#Pool 15 subjects' data
df_combine <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)
dim(df_combine)

head(df_combine)

#Choose random subset of data as training set
training <- sample.int( dim(df_combine)[1], floor(0.60*dim(df_combine))[1], replace=FALSE)

#only keep the median_x, y, and z axis and logarithm of determinant of variance covariance matrix
dataMclust <- Mclust(df_combine[training,3:6])

summary(dataMclust,parameters=TRUE)
summary(dataMclust,classification=TRUE)

#the following provides the classification into 9 clusters.
table(dataMclust$classification[training],df_combine$new_state[training])
dataMclust$classification1[training] <- with(dataMclust, ifelse(dataMclust$classification[training] == 9, 1, 2))
table(dataMclust$classification1[training],df_combine$new_state[training],df_combine$id[training])

#USE MODEL FROM TRAINING SET TO PREDICT CLASSIFICATION OF REMAINING 40% OF DATA
predict40 <- predict(dataMclust, df_combine[-training,3:6])
predict40$classification1 <- with(predict40, ifelse(predict40$classification == 9, 1, 2))
table(predict40$classification1,df_combine$new_state[-training],df_combine$id[-training])

#manually pick the class label, where frequency is most for the raw_state, e.g., if label 1 has most sleep then I say cluster 1 is sleep and then I add the frequencies for the three states based upon this labeling.
#the following command provides the initial true classification.
table(df_combine$new_state)

dataMclust1 <- Mclust(df_combine[,14:17])
summary(dataMclust1,parameters=TRUE)
# plot(dataMclust1)
summary(dataMclust1,classification=TRUE)
table(dataMclust1$classification,df_combine$raw_state)
table(df_combine$raw_state)

dataMclust2 <- Mclust(df_combine[,9])
summary(dataMclust2,parameters=TRUE)
#plot(dataMclust2)
summary(dataMclust2,classification=TRUE)
table(dataMclust2$classification,df_combine$raw_state)
table(df_combine$raw_state)

dataMclust3 <- Mclust(df_combine[,10])
summary(dataMclust3,parameters=TRUE)
#plot(dataMclust3)
summary(dataMclust3,classification=TRUE)
table(dataMclust3$classification,df_combine$raw_state)
table(df_combine$raw_state)