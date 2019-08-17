write.csv_custom<- function(data, name) {
  write.csv(data, paste0(name,"_Full.csv"))
  write.csv(data[,which(names(data)=="Sv_mean")], paste0(name,".csv"))
}

#Acoustics
Herring<-read.csv("D:/Analysis/Chapter3Lunges_Tenakee_Seymour4.csv")
SS2012Krill<-read.csv("D:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")
SS2012Krill<-SS2012Krill[SS2012Krill$Sv_mean_linear>0.0000000001,]
Herring<-read.csv("D:/Analysis/Chapter3Lunges_Tenakee_Seymour4.csv")
TI2013Herring_large<-Herring[(Herring$tag=="mn13_112a"|Herring$tag=="mn13_113a"|Herring$tag=="mn13_111a"|Herring$tag=="mn13_113d" & Herring$dive<=38),]
SC2014Herring<-Herring[startsWith(as.character(Herring$tag),"mn14") & !Herring$tag == "mn14_114a",]
TI2013Herring_small<-Herring[startsWith(as.character(Herring$tag),"mn13")&!(Herring$tag=="mn13_112a"|Herring$tag=="mn13_113a"|Herring$tag=="mn13_111a"|Herring$tag=="mn13_113d" & Herring$dive<=38),]
KB2014Herring<-read.csv("D:/Analysis/Chapter3/Acoustic Data/Herring Regions June2_Sv.csv")
KB2014Herring<-KB2014Herring[1:26,] 
CS2014Chum<-read.csv("Schools_2.csv")
CS2014Chum<-CS2014Chum[c(1:8),]
SC2014Krill<-Herring[Herring$tag == "mn14_114a",]
SC2014Krill$Sv_mean<--62.6
CS2014Coho<- -50
EnergyParameters$Sv<-as.character(EnergyParameters$Sv)
EnergyParameters$Data<-as.character(EnergyParameters$Data)
CS2014Coho<-as.data.frame(CS2014Coho)
CS2014Coho$Sv_mean<--50

write.csv_custom(data = SS2012Krill, name = "SS2012Krill")
write.csv_custom(data = TI2013Herring_large, name = "TI2013Herring_large")
write.csv_custom(data = SC2014Herring, name = "SC2014Herring")
write.csv_custom(data = TI2013Herring_small, name = "TI2013Herring_small")
write.csv_custom(data = KB2014Herring, name = "KB2014Herring")
write.csv_custom(data = CS2014Chum, name = "CS2014Chum")
write.csv_custom(data = SC2014Krill, name = "SC2014Krill")
write.csv_custom(data = CS2014Coho, name = "CS2014Coho")

LengthDistributions<-read.csv("LengthDistributions.csv")


