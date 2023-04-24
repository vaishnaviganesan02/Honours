#allele frequency distribution on R.4.1.0
#high_impact_homogen.frq and low_impact_homogen.frq were converted to .csv files

#low_impact_homogen.csv is assigned to a variable low
low = read.table("low_impact.csv", sep=",", header=T)
#high_impact_homogen.csv is assigned to a variable high
high = read.table("high_impact.csv", sep=",", header=T)

low_freq <- as.numeric(gsub(".*:","", low$X.ALLELE.FREQ..1))
high_freq <- as.numeric(gsub(".*:","", high$X.ALLELE.FREQ..1))

library(ggplot2)

toplot1 = data.frame(A = low_freq)
toplot2 = data.frame(A = high_freq)

low_impact<- ggplot(toplot1, aes(x=-log(A))) + 
  geom_histogram(bins = 10, aes(y = (after_stat(count))/sum(after_stat(count))), fill = "dodgerblue4") +    #plotting allele frequency as -log(allele frequency)
  theme_bw()+
  xlab("-(log(allele frequency))")+
  ylab("frequency")+
  ggtitle("Potential low impact risk variants allele frequency ")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ylim(0.0,0.7)

high_impact<- ggplot(toplot2, aes(x=-log(A))) + 
  geom_histogram(bins = 10, aes(y = (after_stat(count))/sum(after_stat(count))), fill = "firebrick3") +
  theme_bw()+
  xlab("-(log(allele frequency))")+
  ylab("frequency")+
  ggtitle("Potential high impact risk variants allele frequency")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ylim(0.0,0.7)

gridExtra::grid.arrange(low_impact,high_impact, ncol =2) # plotting both plots next to each other

