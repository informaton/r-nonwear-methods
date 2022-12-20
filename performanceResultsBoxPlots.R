library(ggplot2)
resultsPath = "results"

# Change to TRUE if you want to export figures to .tiff files
saveAsTiffs = FALSE

results <- read.csv(file.path(resultsPath, "performanceResults.csv"), header = TRUE, sep=",")
p1<-ggplot(results, aes(x=Method, y=Classification_Error, color=Method)) +
  geom_boxplot(fatten = 2, lwd = 1, outlier.shape=16,
               outlier.size=4 ) +
  labs(x="Method", y = "Classification Error") +  
  theme_bw() +
  theme(legend.position = "none") +   
  theme(axis.text = element_text(size = 8, face = "bold"),axis.title = element_text(size = 12, face = "bold"))
p1 +  scale_colour_brewer("Colors in Dark2", palette="Dark2")
p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
plot(p1)


p2<-ggplot(results, aes(x=Method, y=Precision, color=Method)) +
  geom_boxplot(fatten = 2, lwd = 1, outlier.shape=16,
               outlier.size=4 ) +
  labs(x="Method", y = "Precision") +
  theme_bw() +
  theme(legend.position = "none") +   
  theme(axis.text = element_text(size = 8, face = "bold"), axis.title = element_text(size = 12,face="bold"))
plot(p2 +  scale_colour_brewer("Colors in Dark2", palette="Dark2"))

p3<-ggplot(results, aes(x=Method, y=Recall, color=Method)) +
  geom_boxplot(fatten = 2, lwd = 1, outlier.shape=16,
               outlier.size=4) +
  labs(x="Method", y = "Recall") +
  theme_bw() +
  theme(legend.position = "none") +   
  theme(axis.text = element_text(size = 8, face = "bold"), axis.title = element_text(face="bold", size=12))
plot(p3 +  scale_colour_brewer("Colors in Dark2", palette="Dark2"))


p4<-ggplot(results, aes(x=Method, y=Specificity, color=Method)) +
  geom_boxplot(fatten = 2, lwd = 1, outlier.shape=16,
               outlier.size=4) +
  labs(x="Method", y = "Specificity") +
  theme_bw() +
  theme(legend.position = "none") +   
  theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(face="bold", size=20))
plot(p4 +  scale_colour_brewer("Colors in Dark2", palette="Dark2"))

if(saveAsTiffs){
  ggsave(file.path(resultsPath,"Figure2A.tiff"), plot = p1, device = NULL, dpi=1200)
  ggsave(file.path(resultsPath,"Figure2B.tiff"), plot = p3, device = NULL, dpi=1200)
  ggsave(file.path(resultsPath,"Figure2D.tiff"), plot = p2, device = NULL, dpi=1200)
  ggsave(file.path(resultsPath,"Figure2C.tiff"), plot = p4, device = NULL, dpi=1200)
}
