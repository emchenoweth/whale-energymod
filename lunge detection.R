
setwd("F:/Analysis/Tag processing/JuliaTagData/mn12_262a")
setwd("F:/")
setwd("F:/Analysis/Tags/mn14_110a_from DC")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

is.even <- function(x) x %% 2 == 0

#install.packages("R.matlab")
#install.packages("ggplot2")
library("R.matlab", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("quantmod", lib.loc="~/R/win-library/3.1")

list.files()
data <- readMat("mn14_110a_prhs_1.mat")
data <- as.data.frame(readMat("mn14_110a_prhs_2.mat"))
data$r<-1:nrow(data)

data$MSA<-((data$Aw.1^2+data$Aw.2^2+data$Aw.3^2)^0.5-1)*9.80665 #Burrows et al. 2016
data$jerk<-as.numeric("na")

for(n in 1:length(data$MSA)-1){
data$jerk[n]<- sqrt((data$MSA[n+1]-data$MSA[n])^2)/(1/5)
print(n)
}

data$sec<-data$r/5

setwd("C:/Users/Ellen/Desktop/Trackplot")
dives<-read.csv("AllDivesStats_mn13_109a.csv")
v<-dives$start
dive<-findInterval(data$sec, sort(v))
data$dive<-dive
ggplot()+geom_line(aes(sec,-p,color = as.factor(dive)), data = data[1:6000,])
length(data$speedFN)

---------------------------------
ggplot()+geom_line(aes(r,jerk), data = data[1:1000,])

plot(data$p)
ggplot()+
  geom_line(aes(r,pitch), data = data[1:2000,])+
  geom_line(aes(r,roll), data = data[1:2000,])+
  geom_line(aes(r,head), data = data[1:2000,])

data$p_f<-filter(data$p, rep(1/25,25))
plot(smooth.spline(data$r,-data$p))
ggplot(data = data[1:3000,],aes(r,-p)) +geom_smooth(span = 0.00000001)
peaks<-findPeaks(decimate(-data$p,10))
peaks*10
plot(peaks)
length(peaks)
ggplot()+geom_point()

data$dp<-"na"
data$dp<-as.numeric(data$dp)

for(n in 148281:length(data$p-12)){  #replace starting value with 13
data[n,]$dp<-lm(p~r, data = data[(n-12):(n+12),])$coef[2]
print(n)
}

ggplot()+
  geom_line(aes(r,-p), data = data[1:1300,])+geom_point(aes(r,-p), data = data[data$dp > -0.05 & data$dp < 0.05 & data$p > -0.2 & data$p < 0.1,])

ggplot()+geom_line(aes(r,p_f), data[which(data$p2<50),])
ggplot()+geom_line(aes(r,dp), data[1:3000,])

p1<-ggplot()+geom_line(aes(r,-p), data[1100:1400,])
p2<-ggplot()+geom_point(aes(r,-dp),data[1100:1400,])
multiplot(p1,p2)
#-0.75 breathing threshold
#-50 dive threshold

data$pitch<-data$pitch/(2*pi)*360
data$roll<-data$roll/(2*pi)*360
data$head<-data$head/(2*pi)*360
n<-21
n<-10:20
n<-n+1
p1<-ggplot()+
  geom_line(aes(r,pitch), data = data[data$dive == n & data$p >5,], col = 1)+
  geom_line(aes(r,roll), data = data[data$dive == n & data$p >5,], col = 2)+
  geom_line(aes(r,head), data = data[data$dive == n & data$p >5,], col = 3)
p2<-ggplot()+
  geom_line(aes(sec,-p), data = data[data$dive == n & data$p >5,], col = 4)
p3<-ggplot()+
  geom_line(aes(sec,jerk), data = data[data$dive == n & data$p >5,])+
  scale_y_continuous(limits = c(0, 3))
p4<-ggplot()+
  geom_line(aes(sec,speedFN), data = data[data$dive == n & data$p >5,])+
  scale_y_continuous(limits = c(0.4, 3))
multiplot(p1,p2,p3,p4,cols = 1)


_________________________________
#divide into dives
data$surfacing2<-"na"

data$gap.t<-as.numeric("na")
data$gap.p<-as.numeric("na")

data2<-data[data$dp<0,]
data3<-data2[!is.na(data2$dp),]

for(n in 13:nrow(data3)-1){
  data3$gap.t[n]<-data3$p[n+1]-data3$p[n]
  data3$gap.p[n]<-data3$p[n+1]-data3$p[n]
}

plot(data3$gap.p,data3$gap.t)
hist(data3$gap.p)
hist(data3$gap.t)

data4<-data3[data3$gap.p>100,]
data5<-data4[!is.na(data4$gap.p),]
v<-data5$r
dive<-findInterval(data$r, v)
data$dive<-dive
ggplot()+geom_line(aes(r,-p,color = as.factor(dive)), data = data[1:3000,])

#divide into surface/diving

data$gap.t<-as.numeric("na")
data$gap.p<-as.numeric("na")

data2<-data[data$dp>(-0.01) & data$dp< 0.01,]
data3<-data2[!is.na(data2$dp),]

for(n in 13:nrow(data3)-1){
  data3$gap.t[n]<-data3$p[n-1]-data3$p[n]
  data3$gap.p[n]<-data3$p[n-1]-data3$p[n]
}

plot(data3$gap.p,data3$p)
plot(data3$p,data3$gap.p)
hist(data3$gap.p)
hist(data3$gap.t)

data4<-data3[data3$gap.p>=100,]
data5<-data4[!is.na(data4$gap.p),]
v<-c(v,data5$r)
surfacing2<-findInterval(data$r, sort(v))
data$surfacing2<-surfacing2
ggplot()+geom_point(aes(r,-p,color = as.factor(surfacing2)), data = data[1:3000,])
data$surfacing2<-as.factor(data$surfacing2)
data[is.even(as.numeric(data$surfacing2)),]$surfacing2 <-1
data[!is.even(as.numeric(data$surfacing2)),]$surfacing2 <-2
data$surfacing2<-as.factor(data$surfacing2)
levels(data$surfacing2)<-c("na","surface","diving")
revalue(data$surfacing2, c("1"="surface", "2"="diving"))


peaks
plot()