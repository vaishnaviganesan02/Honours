data<- read.table("pca11_outlier_rm.eigenvec")
data$V12<- NA
data_ids<- as.vector(data$V1)

beijing <- read.table("beijing_samples")
beijing<- as.vector(beijing)
british<- read.table("british_samples")
british<- as.vector(british)
finnish<- read.table("finnish_samples")
finnish<- as.vector(finnish)
genome_asia<- as.vector("genome_asia")

for(i in 1:length(data_ids)){
  if(grepl(data_ids[i],beijing)==TRUE){
    data$V12[i]<- "beijing"
    
  } else if(grepl(data_ids[i],british)){
    data$V12[i]<-"british" 
    
  } else if(grepl(data_ids[i],finnish)==TRUE){
    data$V12[i]<- "finnish"
    
  }else{
    data$V12[i]<-"genome_asia"
  }
  
  i<- i+1
}

#baseplot
data$V12<- as.factor(data$V12)
levels(data$V12)<- c("beijing","british","finnish","genome_asia (indians)")
group_colours<- c("red","cyan","chartreuse2","darkorchid")
plot(data$V2, data$V3,
     xlab = "PC1",
     ylab = "PC2",
     col= group_colours[data$V12],
     pch=16,
     grid())
#ggplot graph
library(ggplot2)
levels(data$V12) <- c("beijing","british","finnish","genome_asia")
group_colours <- c("red","cyan","chartreuse2","darkorchid")
ggplot(data, aes(x = V2, y = V3, color = factor(V12), shape = factor(V12))) +
  geom_point(size = 2, shape = 16) +
  scale_color_manual(values = group_colours,
                     labels=c("beijing","british","finnish","genome_asia (indians)")) +
  xlab("PC1") +
  ylab("PC2") +
  labs(color = "populations") +
  theme(legend.position = "right") +
  theme(panel.grid.major = element_line(color = "white"))+
  theme(plot.background = element_rect(fill = "transparent", color = NA))
  #geom_text(aes(label = V1), hjust = 0.5, vjust = 0.5)


#to get cluster 1 points

x_min <- -0.05
x_max <- -0.0125
y_min <- -0.03
y_max <- 0.01

# Filter the rows corresponding to the rectangular region
cluster1_samples <- subset(data, V2 >= x_min & V2 <= x_max & V3 >= y_min & V3 <= y_max)
cluster1<-as.vector(cluster1_samples$V1)

#to get cluster 2 points
x_min <- -0.0125
x_max <- +0.0125
y_min <- -0.05
y_max <- -0.03

# Filter the rows corresponding to the rectangular region
cluster2_samples1 <- subset(data, V2 >= x_min & V2 <= x_max & V3 >= y_min & V3 <= y_max)
cluster2<-as.vector(cluster2_samples1$V1)

#to get cluster 2 points
x_min <- +0.0125
x_max <- +0.025
y_min <- -0.05
y_max <- -0.02

# Filter the rows corresponding to the rectangular region
cluster2_samples2 <- subset(data, V2 >= x_min & V2 <= x_max & V3 >= y_min & V3 <= y_max)
cluster2 <- c(cluster2,cluster2_samples2$V1)

#to get cluster 2 points
x_min <- +0.0125
x_max <- +0.0375
y_min <- -0.02
y_max <- -0.01

# Filter the rows corresponding to the rectangular region
cluster2_samples3 <- subset(data, V2 >= x_min & V2 <= x_max & V3 >= y_min & V3 <= y_max)
cluster2 <- c(cluster2_samples1$V1,cluster2_samples2$V1,cluster2_samples3$V1)

#to get cluster 2 points
x_min <- +0.025
x_max <- +0.0375
y_min <- -0.01
y_max <- +0.00

# Filter the rows corresponding to the rectangular region
cluster2_samples3 <- subset(data, V2 >= x_min & V2 <= x_max & V3 >= y_min & V3 <= y_max)
cluster2 <- c(cluster2,cluster2_samples2$V1,cluster2_samples3$V1)


print(cluster1)
print(cluster2)
