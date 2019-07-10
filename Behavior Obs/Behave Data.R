setwd("C:/Users/Ellen/Desktop/Analysis/Behavior Obs")
#setwd("/Volumes/Straley1/Analysis/Behavior Obs")
Behave<- read.csv("Humpback Behavior.csv")
#Behave<- read.csv("Behave2perday.csv")
library("gdata", lib.loc="~/R/win-library/3.1")
library("doBy", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("chron", lib.loc="~/R/win-library/3.1")
library(stringi)
Behave<-Behave[Behave$Extra !="x",]
Behave[is.na(Behave$Num_MnHatchery),]$Num_MnHatchery<-0
Behave$Date<- dates(as.character(Behave$Date))
Behave$DOY<-strftime(Behave$Date, format = "%j")
Behave$Start_Time<-times(as.character(Behave$Start_Time))
Behave$End_Time<-times(as.character(Behave$End_Time))
Behave$Year<-as.factor(Behave$Year)
Behave$Hatchery<-as.factor(stri_replace_all_fixed(Behave$Hatchery, " ", ""))
Behave[,"Timing"] <- NA
Behave[,"B.Release"] <- NA
Behave[,"A.Release"] <- NA
Behave[,"Binom_Num_MnHatchery"] <- NA
Behave[,"Predict"] <- NA
Behave<- Behave[Behave$Hatchery != "NotENTERED",]
Behave$Hatchery<-factor(Behave$Hatchery)
Behave$Hatchery<-factor(Behave$Hatchery, levels (Behave$Hatchery)[c(1,5,3,2,4)])
Behave$ObsTime<-Behave$End_Time - Behave$Start_Time
names(Behave)
#Behave<-Behave[Behave$ObsTime<0.01045,]
sum.Behave<-summaryBy(ID1~Hatchery*Date,FUN = length, data = Behave)
sum.Behave[sum.Behave$ID1.length>2,]
Behave<-Behave[Behave$Year!= 1900,]
Behave$Year <- factor(Behave$Year)
Behave$DOY<-as.numeric(Behave$DOY)
#setwd("F:/Analysis")
Releases<-read.csv("Releases20102015_2.csv")
Releases$Date<- dates(as.character(Releases$StartDate))#, format = "%Y/%m/%d")
Releases$StartTime<-times(as.character(Releases$StartTime))
Releases[is.na(Releases$StartTime),]$StartTime<-"8:00:00" #If time is not noted, 8am local time was used.
Releases$Year<-as.factor(Releases$Year)
Releases$Hatchery<-as.factor(stri_replace_all_fixed(Releases$Hatchery, " ", ""))
Releases<-Releases[Releases$Hatchery != "",]
Releases$Hatchery<-factor(Releases$Hatchery)
Releases$Hatchery<-factor(Releases$Hatchery, levels (Releases$Hatchery)[c(1,5,3,2,4)])

#For each observation record, what is relevant release info
  for (h in 1:length(levels(Behave$Hatchery))) {
    for (y in 1:length(levels(Behave$Year))) {
      subset.R<-Releases[Releases$Hatchery == levels(Behave$Hatchery)[h] & Releases$Year == levels(Releases$Year)[y],]
      tryCatch({Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date <  min(subset.R$Date)& !is.na(Behave$Date),]$Timing <-"BeforeRelease"
      }, error = function(e){})
      tryCatch({Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date <  min(subset.R$Date)& !is.na(Behave$Date),]$B.Release <- min(subset.R$Date) - Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date< min(subset.R$Date)& !is.na(Behave$Date),]$Date 
      }, error = function(e){})
      tryCatch({Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date >  max(subset.R$Date)& !is.na(Behave$Date),]$Timing <-"AfterRelease"
      }, error = function(e){})
      tryCatch({Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date <= max(subset.R$Date)& Behave$Date >= min(subset.R$Date)& !is.na(Behave$Date),]$Timing <-"DuringRelease"
      }, error = function(e){})
      tryCatch({Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date >  max(subset.R$Date)& !is.na(Behave$Date),]$A.Release <- Behave[Behave$Hatchery == levels(Behave$Hatchery)[h]& Behave$Year == levels(Behave$Year)[y]& Behave$Date> max(subset.R$Date)& !is.na(Behave$Date),]$Date - max(subset.R$Date)
      }, error = function(e){})
      }
    }

utils::View(Behave)
Behave[Behave$Num_MnHatchery >0 & !is.na(Behave$Num_MnHatchery),]$Binom_Num_MnHatchery<- 1
Behave[Behave$Num_MnHatchery== 0 & !is.na(Behave$Num_MnHatchery),]$Binom_Num_MnHatchery<- 0
Behave$Timing<-as.factor(Behave$Timing)
Behave$Binom_Num_MnHatchery<-as.numeric(Behave$Binom_Num_MnHatchery)
Behave<-Behave[Behave$DOY < 200, ]
Behave$Year<-as.numeric(as.character(Behave$Year))
mod1<-glm(Binom_Num_MnHatchery~Hatchery+Year+Timing+ObsTime, family = binomial, data = Behave)
mod2<-glm(Binom_Num_MnHatchery~Hatchery+poly(Date,2)+Year+ObsTime, family = binomial, data = Behave)
mod3<-glm(Binom_Num_MnHatchery~Hatchery+Timing+ObsTime, family = binomial, data = Behave)

AIC(mod3)
AIC(mod2)
AIC(mod1)
AICc(mod3)
AICc(mod2)
AICc(mod1)
mod<-glm(Binom_Num_MnHatchery~Hatchery+Timing+ObsTime, family = binomial, data = Behave)
mod<-step(mod)
summary(mod)
Behave.mod<-Behave[!is.na(Behave$Binom_Num_MnHatchery)&
                     !is.na(Behave$Hatchery)&
                     !is.na(Behave$Timing)&
                     !is.na(Behave$ObsTime)
                   ,]
r<-nrow(Behave.mod)
Behave.mod[r+1,]$Hatchery<-"PortArmstrong"
Behave.mod[r+1,]$Timing<-"DuringRelease"
Behave.mod[r+1,]$Year<-"2012"
Behave.mod[r+1,]$DOY<-138
Behave.mod$ObsTime<-0.010416667
Behave.mod$Year<-as.numeric(Behave.mod$Year)

Behave.mod$ObsTime<-times(Behave.mod$ObsTime)

Behave.mod$Predict<-predict(mod,newdata = Behave.mod)
Behave.mod$Predict.seHigh<-predict(mod,newdata = Behave.mod)+predict(mod,newdata = Behave.mod, se.fit = TRUE)$se.fit
Behave.mod$Predict.seLow<- predict(mod,newdata = Behave.mod)-predict(mod,newdata = Behave.mod, se.fit = TRUE)$se.fit

mod.a<-glm(Binom_Num_MnHatchery~Hatchery+A.Release+ObsTime+Year, family = binomial(link = "logit"), data = Behave[Behave$Timing == "AfterRelease",])
mod.a<-step(mod.a)
Behave.mod[!is.na(Behave.mod$A.Release),]$Predict<-predict(mod.a,newdata = Behave.mod[!is.na(Behave.mod$A.Release),])
Behave.mod[!is.na(Behave.mod$A.Release),]$Predict.seHigh<-predict(mod.a,newdata = Behave.mod[!is.na(Behave.mod$A.Release),])+predict(mod.a,newdata = Behave.mod[!is.na(Behave.mod$A.Release),],se.fit = TRUE)$se.fit
Behave.mod[!is.na(Behave.mod$A.Release),]$Predict.seLow<-predict(mod.a,newdata = Behave.mod[!is.na(Behave.mod$A.Release),])-predict(mod.a,newdata = Behave.mod[!is.na(Behave.mod$A.Release),],se.fit = TRUE)$se.fit


Releases.Sum<-summaryBy(JulianDateTime ~ Hatchery+Year, FUN = c(min, max), data = Releases)
Releases.Sum<-Releases.Sum[!is.na(Releases.Sum$JulianDateTime.min), ]

Releases.Sum[Releases.Sum$JulianDateTime.min == Releases.Sum$JulianDateTime.max,]$JulianDateTime.max<-Releases.Sum[Releases.Sum$JulianDateTime.min == Releases.Sum$JulianDateTime.max,]$JulianDateTime.max +1


Behave[!is.na(Behave$Year)&!is.na(Behave$Hatchery),]



ggplot()+ theme_bw()+theme(legend.position = "none",axis.text=element_text(size=20),strip.background = element_blank(),strip.text = element_blank()
                           )+
  geom_point(aes(DOY, Binom_Num_MnHatchery,shape = as.factor(Binom_Num_MnHatchery), size = 4), data = Behave.mod)+
  facet_wrap(~Hatchery*Year,ncol = 6, drop=TRUE) + geom_line(aes(DOY,1/(1+exp(-Predict))),data = Behave.mod)+scale_y_log10(breaks = c(0,0.01,0.05,0.15,1.0))+
  geom_rect(data = Releases.Sum, aes(xmin = JulianDateTime.min,xmax = JulianDateTime.max, ymin = 0, ymax = 1), alpha = 0.4)+
  geom_line(aes(DOY, 1/(1+exp(-Predict.seHigh))), data=Behave.mod, lty = 2)+
  geom_line(aes(DOY, 1/(1+exp(-Predict.seLow))), data=Behave.mod, lty = 2)

ggplot()+ theme_bw()+theme(legend.position = "none",axis.text=element_text(size=20),strip.background = element_blank(),strip.text = element_blank()
)+
  geom_point(aes(DOY, Binom_Num_MnHatchery,shape = as.factor(Binom_Num_MnHatchery), size = 4), data = Behave.mod)+
  facet_wrap(~Hatchery*Year,ncol = 6, drop=TRUE) + geom_line(aes(DOY,1/(1+exp(-fit))),data = BehaveNewData2)+scale_y_log10(breaks = c(0,0.01,0.05,0.15,1.0))+
  geom_rect(data = Releases.Sum, aes(xmin = JulianDateTime.min,xmax = JulianDateTime.max, ymin = 0, ymax = 1), alpha = 0.4)+
  geom_line(aes(DOY, 1/(1+exp(-fit.high))), data=BehaveNewData2, lty = 2)+
  geom_line(aes(DOY, 1/(1+exp(-fit.low))), data=BehaveNewData2, lty = 2)

BehaveNewData<-read.csv("BehaveNewData.csv")
BehaveNewData2<-read.csv("BehaveNewData2.csv")
predict(mod.a,newdata = BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",], se.fit = TRUE)
BehaveNewData$ObsTime<-times(BehaveNewData$ObsTime)
BehaveNewData$Predict<-predict(mod,BehaveNewData)
BehaveNewData$Prob<- 1/(1+exp(-BehaveNewData$Predict))
predict(mod,BehaveNewData2,se.fit = TRUE)
BehaveNewData2$fit <-predict(mod,BehaveNewData2,se.fit = TRUE)$fit 
BehaveNewData2$fit.high <-predict(mod,BehaveNewData2,se.fit = TRUE)$se.fit+predict(mod,BehaveNewData2,se.fit = TRUE)$fit 
BehaveNewData2$fit.low <- -predict(mod,BehaveNewData2,se.fit = TRUE)$se.fit+predict(mod,BehaveNewData2,se.fit = TRUE)$fit 
BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",]$fit <-predict(mod.a,BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",],se.fit = TRUE)$fit 
BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",]$fit.high <-predict(mod.a,BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",],se.fit = TRUE)$se.fit+predict(mod,BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",],se.fit = TRUE)$fit 
BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",]$fit.low <- -predict(mod.a,BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",],se.fit = TRUE)$se.fit+predict(mod,BehaveNewData2[BehaveNewData2$Timing == "AfterRelease",],se.fit = TRUE)$fit 
BehaveNewData2$tfit<- 1/(1+exp(-BehaveNewData2$fit))
BehaveNewData2$tfit.high<- 1/(1+exp(-BehaveNewData2$fit.high))
BehaveNewData2$tfit.low<- 1/(1+exp(-BehaveNewData2$fit))
View(BehaveNewData2)
######
Releases.sum2<-summaryBy(StartDate~Hatchery+Year+Species,FUN=c(min, max),data = Releases)
View(Releases.sum2)
Releases.sum3<-summaryBy(Count~Hatchery+Year+Species,FUN=sum,data = Releases)
View(Releases.sum3)
Releases.sum4<-summaryBy(salmonSizeGRAMS~Hatchery+Year+Species,FUN=mean,data = Releases)
View(Releases.sum4)
mean(Releases[Releases$Hatchery == "MistCove" & Releases$Year == "2015" & Releases$Species == "coho" & !is.na(Releases$salmonSizeGRAMS),]$salmonSizeGRAMS)
###############

mod<-glm(Binom_Num_MnHatchery~Hatchery*ObsTime+Timing, family = binomial, data = Behave)
summary(step(mod))

#####
Behave$Breach<-"F"
Behave[grep("breach", Behave$Whale_Behavior),]$Breach<-"T"
Behave[grep("breach", Behave$Comments),]$Breach<-"T"
Behave$Sleep<-"F"
Behave[grep("leep", Behave$Whale_Behavior),]$Sleep<-"T"
Behave[grep("leep", Behave$Comments),]$Sleep<-"T"
Behave$Bubbles<-"F"
Behave[grep("ubble", Behave$Whale_Behavior),]$Bubbles<-"T"
Behave[grep("ubble", Behave$Barrier),]$Bubbles<-"T"
Behave[grep("uble", Behave$Barrier),]$Bubbles<-"T"
Behave[grep("UBBLE", Behave$Barrier),]$Bubbles<-"T"
Behave[grep("ubble", Behave$Comments),]$Bubbles<-"T"
Behave[grep("ubble", Behave$Comments),]$Bubbles<-"T"
Behave[grep("uble", Behave$Comments),]$Bubbles<-"T"
Behave[grep("UBBLE", Behave$Comments),]$Bubbles<-"T"
Behave$Kelp<-"F"
Behave[grep("elp", Behave$Barrier),]$Kelp<-"T"
Behave[grep("elp", Behave$Comments),]$Kelp<-"T"

Behave$Shore<-"F"
Behave[grep("hore", Behave$Barrier),]$Shore<-"T"
Behave[grep("SHORELINE", Behave$Barrier),]$Shore<-"T"
Behave[grep("hore", Behave$Comments),]$Shore<-"T"
Behave[grep("SHORELINE", Behave$Comments),]$Shore<-"T"
Behave$Dock<-"F"
Behave[grep("ock", Behave$Barrier),]$Dock<-"T"
Behave[grep("ens", Behave$Barrier),]$Dock<-"T"
Behave[grep("ENS", Behave$Barrier),]$Dock<-"T"
Behave[grep("ENS", Behave$Comments),]$Dock<-"T"
Behave[grep("ock", Behave$Comments),]$Dock<-"T"
Behave[grep("ens", Behave$Comments),]$Dock<-"T"
Behave$Tide<-"F"
Behave[grep("ide", Behave$Barrier),]$Tide<-"T"
Behave$Surface<-"F"
Behave[grep("urface", Behave$Barrier),]$Surface<-"T"
Behave[grep("lung", Behave$Whale_Behavior),]$Surface<-"T"
Behave[grep("lung", Behave$Comments),]$Surface<-"T"
Behave[grep("urface", Behave$Comments),]$Surface<-"T"


summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Surface))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Tide))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Bubbles))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Sleep))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Breach))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Kelp))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Dock))
summary(as.factor(Behave[Behave$Binom_Num_MnHatchery>0,]$Shore))


Behave$BarrierNum<-as.factor(as.numeric(as.factor(Behave$Bubbles))+as.numeric(as.factor(Behave$Tide))+as.numeric(as.factor(Behave$Kelp))+as.numeric(as.factor(Behave$Dock))+as.numeric(as.factor(Behave$Shore))-5)
summary(Behave[Behave$Binom_Num_MnHatchery>0,]$BarrierNum)




#########

r<-resid(mod)
acf(resid(mod))
n<-length(r)
cor(r[-1],r[-n])
