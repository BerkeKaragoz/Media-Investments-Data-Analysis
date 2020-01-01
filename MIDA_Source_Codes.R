#https://github.com/BerkeKaragoz/Media-Investments-Data-Analysis
#
# E. Berke Karag?z & Sinan Isik
#
# Graph Source Codes
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
#
# MI_Digital_vs_Traditional_TR
# 2010 - 2018 Digital vs Traditional Investments of Turkey
#
###

rdYear <- read.csv("ReklamcilarDernegi_YearlyInvestments.csv")
rdFrame <- data.frame(rdYear)

traditional <- rowSums(rdFrame[, c(2,3,4,5,6)])
digital <- rdFrame$Digital
rdFrame

rd.years <- c("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

specie <- c(rep(rd.years[1], 2) , rep(rd.years[2] , 2) , rep(rd.years[3] , 2) , rep(rd.years[4] , 2), rep(rd.years[5] , 2), rep(rd.years[6] , 2),
            rep(rd.years[7] , 2), rep(rd.years[8] , 2), rep(rd.years[9] , 2))

Type <- rep(c("Traditional" , "Digital"), 9)

value <- c(traditional[1], digital[1], traditional[2], digital[2], traditional[3], digital[3]
           ,traditional[4], digital[4], traditional[5], digital[5], traditional[6], digital[6],
           traditional[7], digital[7], traditional[8], digital[8], traditional[9], digital[9])
data <- data.frame(specie,Type,value)

# Stacked
ggplot(data, aes(fill=Type, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("2010 - 2018 Digital vs Traditional Investments of Turkey", subtitle = "According to Data from Reklamcilar Dernegi") +
  xlab("Years") + ylab("Thousand TL")
###
#
# MI_TR_vs_USA_Digital
# TR vs USA Digital Media Advertisement Expenditures
#
###
# library
library(ggplot2)

# create a dataset
specie <- c(rep("Search Engine", 2) , rep("Banner", 2), rep("Video", 2), rep("Other", 2))
condition <- rep(c("USA", "TR") , 4)

tr <- read.csv("trExpenditures.csv", header=T)
tr$Year <- NULL
trMean <- colMeans(tr)
trMean
trVector <- as.numeric(trMean)
trVector

usa <- read.csv("usaExpenditures.csv", header=T)
usa$Year <- NULL
usaMean <- colMeans(usa)
usaMean
usaVector <- as.numeric(usaMean)
usaVector


value <- c(usaVector[1], trVector[1],usaVector[2], trVector[2],usaVector[3], trVector[3],usaVector[4], trVector[4])
value

data <- data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") + scale_y_log10() +
  scale_fill_manual("Countries", values= c("USA" = "#3C3B6E", "TR" = "red"))+
  ggtitle("TR vs USA Digital Media Advertisement Expenditures", subtitle = "According to 2017 and 2018 First Halves Means Recompiled Data from IAB as Log Values") + xlab("Medium") + ylab("Million $")
###
#
###################################################################
#
# These codes are used for descriptive stats:
#
library(Hmisc)

rd <- read.csv("rd2.csv", header=T)
rd$Years <- NULL

summary(rd)
DS <- describe(rd)
DS

round(sd(rd$Digital),2)

describe(rd)