# Master Script that follows calculations in the paper

library(accelerometR)
library(ggplot2)
library(reshape2)

acclR_data$raw_state <- as.numeric(acclR_data$raw_state)

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
s16 <- c(1:60,120:1572,1633:1693)
df16 <- acclR_data[acclR_data$id == "100602" & acclR_data$timestep %in% s16, ]

df_combine <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)

# *****************************Table 1:******************************************
# proportion of time spent in each activity state:
propNW <- dim(df_combine[df_combine$raw_state==1,])[1]/dim(df_combine)[1]
propS <- dim(df_combine[df_combine$raw_state==2,])[1]/dim(df_combine)[1]
propW <- dim(df_combine[df_combine$raw_state==3,])[1]/dim(df_combine)[1]
cat("Percent of time in nonwear:", propNW, "\nPercent of time in sleep:", propS, "\nPercent of time in wear:", propW)

# mean and SD for x,y,z axes counts for each activity state
round(mean(df_combine$x_axis[df_combine$raw_state==1]),4)
round(sd(df_combine$x_axis[df_combine$raw_state==1]),4)
round(mean(df_combine$x_axis[df_combine$raw_state==2]),4)
round(sd(df_combine$x_axis[df_combine$raw_state==2]),4)
round(mean(df_combine$x_axis[df_combine$raw_state==3]),4)
round(sd(df_combine$x_axis[df_combine$raw_state==3]),4)

round(mean(df_combine$y_axis[df_combine$raw_state==1]),4)
round(sd(df_combine$y_axis[df_combine$raw_state==1]),4)
round(mean(df_combine$y_axis[df_combine$raw_state==2]),4)
round(sd(df_combine$y_axis[df_combine$raw_state==2]),4)
round(mean(df_combine$y_axis[df_combine$raw_state==3]),4)
round(sd(df_combine$y_axis[df_combine$raw_state==3]),4)

round(mean(df_combine$z_axis[df_combine$raw_state==1]),4)
round(sd(df_combine$z_axis[df_combine$raw_state==1]),4)
round(mean(df_combine$z_axis[df_combine$raw_state==2]),4)
round(sd(df_combine$z_axis[df_combine$raw_state==2]),4)
round(mean(df_combine$z_axis[df_combine$raw_state==3]),4)
round(sd(df_combine$z_axis[df_combine$raw_state==3]),4)

# mean and SD for median vector counts for each activity state
round(mean(df_combine$L2_norm[df_combine$raw_state==1]),4)
round(sd(df_combine$L2_norm[df_combine$raw_state==1]),4)
round(mean(df_combine$L2_norm[df_combine$raw_state==2]),4)
round(sd(df_combine$L2_norm[df_combine$raw_state==2]),4)
round(mean(df_combine$L2_norm[df_combine$raw_state==3]),4)
round(sd(df_combine$L2_norm[df_combine$raw_state==3]),4)

# ****************INSERT Table 2: QDA ANALYSIS*****************

# ****************HMM Analysis*****************
  #do HMM on 1 subject->get vectors of predicted, actual
  #send to compute_class_tables for that one ID
  #repeat
# ****************GMM Analysis*****************
  #do GMM for all subjects->
  #figure out what to do with the ID part of the compute_class_tables



# ******************Figure Generation******************

# Figure 1:
vec=c(2,7,3,4,5,6)
acclR_data135 <- acclR_data[acclR_data$id == "100135",]
head(acclR_data135)
acclR_data135 <- acclR_data135[,vec]
colnames(acclR_data135) <- c("Time_Interval", "Activity State", "Centroid X Axis","Centroid Y Axis","Centroid Z Axis","Log of Determinant of Covariance Matrix")
acclR_data135$Time_Interval <- acclR_data135$Time_Interval/120
df_melt = melt(acclR_data135, id.vars = 'Time_Interval')
colnames(df_melt) <- c("Time_Interval","variable","Acceleration")
plot1 <- ggplot(df_melt, aes(x = Time_Interval, y = Acceleration)) + 
  geom_line()  + 
  facet_wrap(~ variable, scales = 'free_y', ncol = 1)
plot1 + labs(x = "Time Intervals (hours)")
ggsave("Figure1.tiff", plot1 + labs(x = "Time Intervals (hours)"), device = NULL, dpi=1200)