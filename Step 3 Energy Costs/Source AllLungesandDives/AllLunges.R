library("chron", lib.loc="~/R/win-library/3.5")



AllLunges<- rbind.fill(read.csv("mn13_108a_lunge_summaryA.csv")
                       ,read.csv("mn13_109a_lunge_summaryA.csv")
                       ,read.csv("mn13_110a_lunge_summaryA.csv")
                       ,read.csv("mn13_111a_lunge_summaryA.csv")
                       ,read.csv("mn13_112a_lunge_summaryA.csv")
                       ,read.csv("mn13_113a_lunge_summaryA.csv")
                       ,read.csv("mn13_113d_lunge_summaryA.csv")
                       ,read.csv("mn14_110a_lunge_summaryA.csv")
                       ,read.csv("mn14_113a_lunge_summaryA.csv")
                       ,read.csv("mn14_113b_lunge_summaryA.csv")
                       ,read.csv("mn14_114a_lunge_summaryA.csv")
                       ,read.csv("mn12_262b_lunge_summaryA.csv")
                       ,read.csv("mn12_262a_lunge_summaryA.csv"))

# Then you need to open in excel and reformat dates for R package Chron

#AllLunges$dive<-as.numeric(as.character(AllLunges$dive))
#AllLunges<-AllLunges[,-(29:102)]
AllLunges$DateTime<-as.character(AllLunges$DateTime)
AllLunges$Time<- times(format(as.POSIXlt(AllLunges$DateTime, tz ="America/Anchorage"), "%H:%M:%S"))
AllLunges[is.na(AllLunges$max.speed),]
AllLunges[AllLunges$tag == "mn13_110a" & AllLunges$dive == 69,]$max.speed<-AllLunges[AllLunges$tag == "mn13_110a" & AllLunges$dive == 71,]$max.speed
AllLunges[AllLunges$tag == "mn14_113b" & AllLunges$dive == 420,]$max.speed<-mean(AllLunges[AllLunges$tag == "mn14_113b" & AllLunges$dive == 420,]$max.speed, na.rm = TRUE)
AllLunges[AllLunges$tag == "mn14_113b" & AllLunges$dive == 420,]$max.speed<-mean(AllLunges[AllLunges$tag == "mn14_113b" & AllLunges$dive == 420,]$max.speed, na.rm = TRUE)
AllLunges[121,]$min.speed<-1
write.csv(AllLunges, "AllLunges.csv")
