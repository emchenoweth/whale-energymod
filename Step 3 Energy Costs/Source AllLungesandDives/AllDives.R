setwd("C:/Users/emchenoweth/Desktop/New folder/Step 3 Energy Costs/Source AllLungesandDives")


mn13_108a_diveSUMA<-read.csv("mn13_108a_diveSUMA.csv")
mn13_108a_diveSUMA$tag<- "mn13_108a"
mn13_109a_diveSUMA<-  read.csv("mn13_109a_diveSUMA.csv")
mn13_109a_diveSUMA$tag<- "mn13_109a"
mn13_110a_diveSUMA<-    read.csv("mn13_110a_diveSUMA.csv")
mn13_110a_diveSUMA$tag<-"mn13_110a"
mn13_111a_diveSUMA<-    read.csv("mn13_111a_diveSUMA.csv")
mn13_111a_diveSUMA$tag<-"mn13_111a"
mn13_112a_diveSUMA<-     read.csv("mn13_112a_diveSUMA.csv")
mn13_112a_diveSUMA$tag<-"mn13_112a"
mn13_113a_diveSUMA<-     read.csv("mn13_113a_diveSUMA.csv")
mn13_113a_diveSUMA$tag<-"mn13_113a"
mn13_113d_diveSUMA<-     read.csv("mn13_113d_diveSUMA.csv")
mn13_113d_diveSUMA$tag<-"mn13_113d"
mn14_110a_diveSUMA<-     read.csv("mn14_110a_diveSUMA.csv")
mn14_110a_diveSUMA$tag<-"mn14_110a"
mn14_113a_diveSUMA<-    read.csv("mn14_113a_diveSUMA.csv")
mn14_113a_diveSUMA$tag<-"mn14_113a"
mn14_113b_diveSUMA<-    read.csv("mn14_113b_diveSUMA.csv")
mn14_113b_diveSUMA$tag<-"mn14_113b"
mn14_114a_diveSUMA<-     read.csv("mn14_114a_diveSUMA.csv")
mn14_114a_diveSUMA$tag<-"mn14_114a"
mn12_diveSUMA<-     read.csv("mn12_diveSUMA2.csv")
mn12_262b_diveSUMA<-     read.csv("mn12_262b_diveSUMA.csv")
mn12_262a_diveSUMA<-     read.csv("mn12_262a_diveSUMA.csv")
mn12_262a_diveSUMA$tag<-"mn12_262a"

mn13_108a_diveSUMA<-read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_108a_diveSUMA.csv")
mn13_108a_diveSUMA$tag<- "mn13_108a"
mn13_109a_diveSUMA<-  read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_109a_diveSUMA.csv")
mn13_109a_diveSUMA$tag<- "mn13_109a"
mn13_110a_diveSUMA<-    read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_110a_diveSUMA.csv")
mn13_110a_diveSUMA$tag<-"mn13_110a"
mn13_111a_diveSUMA<-    read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_111a_diveSUMA.csv")
mn13_111a_diveSUMA$tag<-"mn13_111a"
mn13_112a_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_112a_diveSUMA.csv")
mn13_112a_diveSUMA$tag<-"mn13_112a"
mn13_113a_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_113a_diveSUMA.csv")
mn13_113a_diveSUMA$tag<-"mn13_113a"
mn13_113d_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn13_113d_diveSUMA.csv")
mn13_113d_diveSUMA$tag<-"mn13_113d"
mn14_110a_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn14_110a_diveSUMA.csv")
mn14_110a_diveSUMA$tag<-"mn14_110a"
mn14_113a_diveSUMA<-    read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn14_113a_diveSUMA.csv")
mn14_113a_diveSUMA$tag<-"mn14_113a"
mn14_113b_diveSUMA<-    read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn14_113b_diveSUMA.csv")
mn14_113b_diveSUMA$tag<-"mn14_113b"
mn14_114a_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn14_114a_diveSUMA.csv")
mn14_114a_diveSUMA$tag<-"mn14_114a"
mn12_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn12_diveSUMA2.csv")
mn12_262b_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn12_262b_diveSUMA.csv")
mn12_262a_diveSUMA<-     read.csv("D:/Analysis/Chapter3/Tags/Chapter3Lunges/mn12_262a_diveSUMA.csv")
mn12_262a_diveSUMA$tag<-"mn12_262a"

AllDives<-rbind.fill(mn13_108a_diveSUMA,
                     mn13_109a_diveSUMA,
                     mn13_110a_diveSUMA,
                     mn13_111a_diveSUMA,
                     mn13_112a_diveSUMA,
                     mn13_113a_diveSUMA,
                     mn13_113d_diveSUMA,
                     mn14_110a_diveSUMA,
                     mn14_113a_diveSUMA,
                     mn14_113b_diveSUMA,
                     mn14_114a_diveSUMA,
                     mn12_diveSUMA,
                     mn12_262b_diveSUMA,
                     mn12_262a_diveSUMA)

write.csv(AllDives, "AllDives.csv")
