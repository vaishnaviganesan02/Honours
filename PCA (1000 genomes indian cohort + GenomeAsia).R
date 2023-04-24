library(ggfortify)
library(ggplot2)
library(dplyr)

data<- read.table("pca2.eigenvec")
#creating a new column to create levels
data$V12<- NA

telugu<- grep("HG",data$V1)

genome_asia<- grep("GA",data$V1)

gujarati<- grep("NA",data$V1)

for(i in genome_asia){
  data$V12[i]<- "genome_asia"
}

for (i in telugu) {
  data$V12[i]<- "telugu"
}

for(i in gujarati){
  data$V12[i]<- "gujarati"
}

data$V12<- as.factor(data$V12)
levels(data$V12)<- c("genome_asia","telugu","gujarati")
group_colours <- c("darkorchid","darkolivegreen","darkred")
ggplot(data, aes(x = V2, y = V3, color = factor(V12), shape = factor(V12))) +
  geom_point(size = 2, shape = 16) +
  scale_color_manual(values = group_colours,
                     labels=c("genome_asia (indians)","telugu (1000 genome)",
                              "gujarati ( 1000 genome)")) +
  xlab("PC1") +
  ylab("PC2") +
  labs(color = "populations") +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(color = "white"))+
  theme(plot.background = element_rect(fill = "transparent", color = NA)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  ggtitle("PCA on GenomeAsia and 1000 genomes Indian cohort")
