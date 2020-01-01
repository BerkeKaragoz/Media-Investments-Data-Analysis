#https://github.com/BerkeKaragoz/Media-Investments-Data-Analysis
#
# E. Berke Karagöz & Sinan Isik
#
#Graph Source Codes
#
####################################################################
#
# MI_Distribution_TR
#2010 - 2018 Media Investments Distribution of Turkey by Medium
#
####
rd <- read.csv(file = "rd2.csv")
rdRaw <- rd
rdRaw$Years <- NULL
rdRaw

rdOneDimension <- c(rdRaw$TV, rdRaw$Press, rdRaw$Outdoor, rdRaw$Radio, rdRaw$Cinema, rdRaw$Digital)
rdOneDimension

specie <- rep(2018:2010, 6)
specie

Medium <- c(rep("TV", 9), rep("Press", 9),rep("Outdoor", 9),rep("Radio", 9),rep("Cinema", 9), rep("Digital", 9))
Medium

data <- data.frame(specie, Medium, rdOneDimension)

library(ggplot2)

cls <- rep(c("blue", "cyan", "red", "green", "black", "orange"), 9);
ggplot(data, aes(fill=Medium, y=rdOneDimension, x=specie)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("2010 - 2018 Media Investments Distribution of Turkey by Medium", subtitle = "According to Data from Reklamcilar Dernegi") +
  xlab("Years") + ylab("Distribution") +
  scale_x_continuous(breaks=specie) +
  scale_fill_manual(values=cls)
###
#
#