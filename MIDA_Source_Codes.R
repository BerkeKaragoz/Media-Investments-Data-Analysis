#https://github.com/BerkeKaragoz/Media-Investments-Data-Analysis
#
# E. Berke Karagöz & Sinan Isik
#
#Graph Source Codes
#
####################################################################
#
# MI_Distribution_TR
# 2010 - 2018 Media Investments Distribution of Turkey by Medium
#
####
rd <- read.csv(file = "ReklamcilarDernegi_YearlyInvestments.csv")
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
# IUP_TR_vs_USA_2019
# 2019 Estimates of Internet Usage and Population for TR and USA
#
###
usaInt <- read.csv(file="usaInternet.csv", header=T)
usaInt

trInt <- read.csv(file="trInternet.csv", header=T)
trInt

usaInt$Population.Penetration <- trInt$Population.Penetration <- NULL

internet <- rbind(usaInt, trInt)
internet

value <- c(internet$Population, internet$Internet.Usage, internet$Facebook)
condition <- c(rep("Population", 2), rep("Internet Users", 2), rep("Facebook",2) )
specie <- rep(c("USA", "TR"), 3)

data <- data.frame(specie, condition, value)

cls <- rep(c("#3b5998", "cyan", "yellow"), 2);

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("2019 Estimates of Internet Usage and Population for TR and USA", subtitle = "According to Data from Internet World Stats") +
  xlab("Countries") + ylab("Million Amount") + scale_fill_manual(values = cls) + labs(fill="")
###