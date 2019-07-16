library(readr)
NewFig <- read.csv("NewFig!.csv")
AllLunges <- read.csv("AllLunges.csv")

##Prey Escape Model





##Whale Tactics Model






##metabolic costs for surface and subsurface lunges)
AllLunges$tag<-as.factor(AllLunges$tag)
LungeSC<-AllLunges[AllLunges$tag == "mn14_110a"|AllLunges$tag == "mn14_113a"|AllLunges$tag == "mn14_113b",]
mean(LungeSC[LungeSC$max.p>=22.5,]$meta.cost)
mean(LungeSC[LungeSC$max.p<22.5,]$meta.cost)

DivesSC<-AllDives[AllDives$tag == "mn14_110a"|AllDives$tag == "mn14_113a"|AllDives$tag == "mn14_113b",]
DivesSC<-DivesSC[DivesSC$p.max<22.5,]
summary(DivesSC)
names(DivesSC)
mean(DivesSC$total.meta.cost/(DivesSC$dur/60), na.rm = TRUE)

DivesSC<-AllDives[AllDives$tag == "mn13_109a"|AllDives$tag == "mn13_108a"|AllDives$tag == "mn14_110a"|AllDives$tag == "mn14_113a"|AllDives$tag == "mn14_113b",]
DivesSC<-DivesSC[DivesSC$p.max>=22.5,]
summary(DivesSC)
names(DivesSC)
mean(DivesSC$total.meta.cost/(DivesSC$dur/60), na.rm = TRUE)

#New.Fig<- read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/NewFig!.csv")
New.Fig<- read.csv("NewFig!.csv")
New.Fig<-New.Fig[1:13,]
devtools::install_github("seananderson/ggsidekick")

library(ggsidekick)

#library("ggplot2", lib.loc="~/R/win-library/3.4")
library("ggrepel", lib.loc="~/R/win-library/3.5")
library("gridExtra", lib.loc="~/R/win-library/3.5")
#Functions
Data<- New.Fig
x1<-New.Fig$B-New.Fig$null*24.8*0.84
y1<-1:nrow(New.Fig)
x2<-New.Fig$B
xlab1<-"delta (kJ/lunge) with prey escape"
xlab2<-"KJ/lunge"
shape<-New.Fig$depth
color<-New.Fig$species
anomaly.fig <- function(Data,x,y) {
  p1<-ggplot(Data) +
    theme_sleek()+
    theme(text = element_text(size=10),
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank())+
    geom_point(aes(x1,y1, color = color, shape = shape), size =3) +
    #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
    #theme_classic(base_size = 10)+
    #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
    ylab("")+xlab(xlab1)+geom_segment(y = 1:nrow(Data), yend = 1:nrow(Data),xend = 0,x =x1)
  
  p2<-ggplot(Data)+
    geom_point(aes(x2,1, color = color, shape = shape), size =3) +
    scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank())+
    ylab("")+xlab(xlab2)
  
}


#Null Model
New.Fig$null<-New.Fig$patch.energy.density*24.8*0.84

ggplot(New.Fig) +
  geom_point(aes(patch.energy.density,null, color = species), size =3) +
  geom_text_repel(aes(patch.energy.density, null, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_smooth(aes(patch.energy.density,null),method = "lm", lty = 3, se = FALSE)+
  ylab("")+xlab("null model (kJ/lunge)")

##!!!##
ggplot(New.Fig) +
  geom_point(aes(1,null, color = species), size =3) +
  #geom_text_repel(aes(1, null, label = New.Fig$X, color = species)) +
  #theme_classic(base_size = 20)+
  #geom_smooth(aes(patch.energy.density,null),method = "lm", lty = 3, se = FALSE)+
  ylab("null model (kJ/lunge)")+xlab("")+theme_sleek()+theme(text = element_text(size=20))


#Prey and whale capture prop with mean whale beahviors (all subsurface, mean speed, single)
var<-read.csv("Chapter2Model2.csv")
EnergyParameters<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/Chapter3ModParValues.csv")
EnergyParameters$CaptureProp_nowhalebehave<-NA
New.Fig$B<-New.Fig$prop.captured..no.whale.behavior*New.Fig$null
ggplot(New.Fig) +
  geom_point(aes(patch.energy.density, B, color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("Escape (kJ/lunge)")+xlab("Energy distribution (kJ/m3)")
New.Fig$null.Sq<- (New.Fig$null-mean(New.Fig$null))^2
New.Fig$B.Sq<- ((New.Fig$null*(mean(New.Fig$prop.captured..no.whale.behavior)))- New.Fig$B)^2

##!!!##
New.Fig<- New.Fig[order(New.Fig$null),]
ggplot(New.Fig) +
  theme_sleek()+
  theme(text = element_text(size=10),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  geom_point(aes(New.Fig$B-New.Fig$null*24.8*0.84,1:nrow(New.Fig), color = species, shape = New.Fig$depth), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  #theme_classic(base_size = 10)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("")+xlab("delta (kJ/lunge) with prey escape")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),xend = 0,x =New.Fig$B-New.Fig$null*24.8*0.84)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$B,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")
  
  

# with whale behavior
New.Fig$C<-New.Fig$prop.captured*New.Fig$patch.energy.density*24.8*0.84
ggplot(New.Fig) +
  geom_point(aes(B,C , color = species), size =3) +
  #geom_text_repel(aes(B, C, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = 0, slope = 1)+
  ylab("Tactics (net kJ/lunge)")+xlab("Escape (kJ/lunge)")
New.Fig$C.Sq<-New.Fig$C-New.Fig$B

##!!!##
#New.Fig<- New.Fig[order(New.Fig$C),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$C-New.Fig$B,1:nrow(New.Fig), color = New.Fig$species, shape = depth), size = 3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  theme(text = element_text(size=10),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("")+xlab("delta (kJ/lunge) with tactics")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$C-New.Fig$B)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$C,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")

#energy/m3 - metabolic costs and prop captured per lunge

New.Fig$D<-New.Fig$prop.captured*New.Fig$patch.energy.density*24.8*0.84-as.numeric(New.Fig$metabolic.costs_lunge)
ggplot(New.Fig) +
  geom_point(aes(C,D , color = species), size =3) +
  #geom_text_repel(aes(C, D, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = -mean(New.Fig$metabolic.costs_lunge), slope = 1)+
  ylab("Metabolic costs (net kJ/lunge)")+xlab("Tactics (kJ/lunge)")
New.Fig$D.Sq<-New.Fig$C-(New.Fig$C-mean(New.Fig$metabolic.costs_lunge))

##!!!##
New.Fig<- New.Fig[order(New.Fig$D),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$D-New.Fig$C,1:nrow(New.Fig), color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  scale_y_continuous(breaks=1:13)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("ranking")+xlab("change in (kJ/lunge) when considering metabolic costs")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$D-New.Fig$C)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$D,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")

#energy/m3 - metabolic costs (100% captured assumed) per min
ggplot(New.Fig) +
  geom_point(aes(D,net.energy.gain_min , color = species), size =3) +
  #geom_text_repel(aes(C, net.energy.gain_min, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = -55.608, slope = mean(New.Fig$lunge.rate))+
  #geom_smooth(aes(C,net.energy.gain_min),method = "lm", lty = 3, se = FALSE)+
  ylab("Lunge Rate (net kJ/min)")+xlab("Metabolic costs (net kJ/lunge)")

##!!!##
New.Fig<- New.Fig[order(New.Fig$net.energy.gain_min),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate),1:nrow(New.Fig), color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  scale_y_continuous(breaks=1:13)+
  geom_vline(xintercept = -250/12.2*60,lty=2)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("ranking")+xlab("change in (kJ/min) when considering lunge rate")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate))

ggplot(New.Fig)+
  geom_point(aes(New.Fig$net.energy.gain_min,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/min)")


New.Fig$NEG.Sq<- New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate)-55.608
  
SSq.null<-sum(abs(New.Fig$null.Sq))
SSq.B<-sum(abs(New.Fig$B.Sq))
SSq.C<-sum(abs(New.Fig$C.Sq))
SSq.D<-sum(abs(New.Fig$D.Sq))
SSq.NEG<-sum(abs(New.Fig$NEG.Sq))
####

New.Fig2<-melt(New.Fig[,c(1,3,4,14,15,18,20,13)], id=c("X","species","depth") )
New.Fig2$rank<-NA

New.Fig2$variable = factor(New.Fig2$variable,levels(New.Fig2$variable)[c(5,4,3,2,1)])
levels(New.Fig2$variable)<-c("lunge rates","metabolic costs","tactics","prey capture","energy distribution")
for (n in levels(New.Fig2$variable)[4:5]){
  for (x in 1:length(New.Fig2[New.Fig2$variable == n,]$rank)){
    New.Fig2[New.Fig2$variable == n,]$rank[x]<-length(unique(New.Fig2[New.Fig2$variable == n & New.Fig2$value > New.Fig2[New.Fig2$variable == n,]$value[x],]$value))+3
  }
  }
 
for (n in levels(New.Fig2$variable)[1:3]){
  for (x in 1:length(New.Fig2[New.Fig2$variable == n,]$rank)){
    New.Fig2[New.Fig2$variable == n,]$rank<-14-rank(New.Fig2[New.Fig2$variable == n,]$value)
  }
}

S_sqrt <- function(x){sign(x)*(abs(x))^(1/2)}
S_cbrt <- function(x){sign(x)*(abs(x))^(1/10)}
IS_cbrt <- function(x){x^10*sign(x)}
IS_sqrt <- function(x){x^(2)*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
S_cbrt_trans <- function() trans_new("S_cbrt",S_cbrt,IS_cbrt)

ggplot()+geom_point(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)), color = New.Fig2$species))+
  #scale_x_continuous(limits = c(-2000,20000))+
  theme_sleek()+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)),group = New.Fig2$X,color = New.Fig2$species))
 # scale_x_log10() 

ggplot()+ 
  geom_point(aes(New.Fig2$rank,New.Fig2$variable, color = New.Fig2$species), size = 3)+
  #geom_text(aes(New.Fig2[New.Fig2$variable == "tactics",]$rank,New.Fig2[New.Fig2$variable == "tactics",]$variable, label = New.Fig2[New.Fig2$variable == "tactics",]$depth), size = 3)+
  scale_x_reverse()+
  theme_sleek()+
  xlab("rank")+ ylab("model")+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$rank,New.Fig2$variable,group = New.Fig2$X,color = New.Fig2$species))
# scale_x_log10() 

ggplot()+geom_point(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable))))+
  scale_x_reverse()+
  theme_sleek()+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)),group = New.Fig2$X,color = New.Fig2$species))
# scale_x_log10() 

=======

##metabolic costs for surface and subsurface lunges)
AllLunges$tag<-as.factor(AllLunges$tag)
LungeSC<-AllLunges[AllLunges$tag == "mn14_110a"|AllLunges$tag == "mn14_113a"|AllLunges$tag == "mn14_113b",]
mean(LungeSC[LungeSC$max.p>=22.5,]$meta.cost)
mean(LungeSC[LungeSC$max.p<22.5,]$meta.cost)

DivesSC<-AllDives[AllDives$tag == "mn14_110a"|AllDives$tag == "mn14_113a"|AllDives$tag == "mn14_113b",]
DivesSC<-DivesSC[DivesSC$p.max<22.5,]
summary(DivesSC)
names(DivesSC)
mean(DivesSC$total.meta.cost/(DivesSC$dur/60), na.rm = TRUE)

DivesSC<-AllDives[AllDives$tag == "mn13_109a"|AllDives$tag == "mn13_108a"|AllDives$tag == "mn14_110a"|AllDives$tag == "mn14_113a"|AllDives$tag == "mn14_113b",]
DivesSC<-DivesSC[DivesSC$p.max>=22.5,]
summary(DivesSC)
names(DivesSC)
mean(DivesSC$total.meta.cost/(DivesSC$dur/60), na.rm = TRUE)

New.Fig<- read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/NewFig!.csv")
New.Fig<-New.Fig[1:13,]
devtools::install_github("seananderson/ggsidekick")

library(ggsidekick)

#library("ggplot2", lib.loc="~/R/win-library/3.4")
library("ggrepel", lib.loc="~/R/win-library/3.4")
library("gridExtra", lib.loc="~/R/win-library/3.4")
#Functions
Data<- New.Fig
x1<-New.Fig$B-New.Fig$null*24.8*0.84
y1<-1:nrow(New.Fig)
x2<-New.Fig$B
xlab1<-"delta (kJ/lunge) with prey escape"
xlab2<-"KJ/lunge"
shape<-New.Fig$depth
color<-New.Fig$species
anomaly.fig <- function(Data,x,y) {
  p1<-ggplot(Data) +
    theme_sleek()+
    theme(text = element_text(size=10),
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank())+
    geom_point(aes(x1,y1, color = color, shape = shape), size =3) +
    #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
    #theme_classic(base_size = 10)+
    #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
    ylab("")+xlab(xlab1)+geom_segment(y = 1:nrow(Data), yend = 1:nrow(Data),xend = 0,x =x1)
  
  p2<-ggplot(Data)+
    geom_point(aes(x2,1, color = color, shape = shape), size =3) +
    scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank())+
    ylab("")+xlab(xlab2)
  
}


#Null Model
New.Fig$null<-New.Fig$patch.energy.density*24.8*0.84

ggplot(New.Fig) +
  geom_point(aes(patch.energy.density,null, color = species), size =3) +
  geom_text_repel(aes(patch.energy.density, null, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_smooth(aes(patch.energy.density,null),method = "lm", lty = 3, se = FALSE)+
  ylab("")+xlab("null model (kJ/lunge)")

##!!!##
ggplot(New.Fig) +
  geom_point(aes(1,null, color = species), size =3) +
  #geom_text_repel(aes(1, null, label = New.Fig$X, color = species)) +
  #theme_classic(base_size = 20)+
  #geom_smooth(aes(patch.energy.density,null),method = "lm", lty = 3, se = FALSE)+
  ylab("null model (kJ/lunge)")+xlab("")+theme_sleek()+theme(text = element_text(size=20))


#Prey and whale capture prop with mean whale beahviors (all subsurface, mean speed, single)
var<-read.csv("Chapter2Model2.csv")
EnergyParameters<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/Chapter3ModParValues.csv")
EnergyParameters$CaptureProp_nowhalebehave<-NA
New.Fig$B<-New.Fig$prop.captured..no.whale.behavior*New.Fig$null
ggplot(New.Fig) +
  geom_point(aes(patch.energy.density, B, color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("Escape (kJ/lunge)")+xlab("Energy distribution (kJ/m3)")
New.Fig$null.Sq<- (New.Fig$null-mean(New.Fig$null))^2
New.Fig$B.Sq<- ((New.Fig$null*(mean(New.Fig$prop.captured..no.whale.behavior)))- New.Fig$B)^2

##!!!##
New.Fig<- New.Fig[order(New.Fig$null),]
ggplot(New.Fig) +
  theme_sleek()+
  theme(text = element_text(size=10),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  geom_point(aes(New.Fig$B-New.Fig$null*24.8*0.84,1:nrow(New.Fig), color = species, shape = New.Fig$depth), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  #theme_classic(base_size = 10)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("")+xlab("delta (kJ/lunge) with prey escape")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),xend = 0,x =New.Fig$B-New.Fig$null*24.8*0.84)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$B,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")
  
  

# with whale behavior
New.Fig$C<-New.Fig$prop.captured*New.Fig$patch.energy.density*24.8*0.84
ggplot(New.Fig) +
  geom_point(aes(B,C , color = species), size =3) +
  #geom_text_repel(aes(B, C, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = 0, slope = 1)+
  ylab("Tactics (net kJ/lunge)")+xlab("Escape (kJ/lunge)")
New.Fig$C.Sq<-New.Fig$C-New.Fig$B

##!!!##
#New.Fig<- New.Fig[order(New.Fig$C),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$C-New.Fig$B,1:nrow(New.Fig), color = New.Fig$species, shape = depth), size = 3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  theme(text = element_text(size=10),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("")+xlab("delta (kJ/lunge) with tactics")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$C-New.Fig$B)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$C,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")

#energy/m3 - metabolic costs and prop captured per lunge

New.Fig$D<-New.Fig$prop.captured*New.Fig$patch.energy.density*24.8*0.84-as.numeric(New.Fig$metabolic.costs_lunge)
ggplot(New.Fig) +
  geom_point(aes(C,D , color = species), size =3) +
  #geom_text_repel(aes(C, D, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = -mean(New.Fig$metabolic.costs_lunge), slope = 1)+
  ylab("Metabolic costs (net kJ/lunge)")+xlab("Tactics (kJ/lunge)")
New.Fig$D.Sq<-New.Fig$C-(New.Fig$C-mean(New.Fig$metabolic.costs_lunge))

##!!!##
New.Fig<- New.Fig[order(New.Fig$D),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$D-New.Fig$C,1:nrow(New.Fig), color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  scale_y_continuous(breaks=1:13)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("ranking")+xlab("change in (kJ/lunge) when considering metabolic costs")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$D-New.Fig$C)

ggplot(New.Fig)+
  geom_point(aes(New.Fig$D,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/lunge)")

#energy/m3 - metabolic costs (100% captured assumed) per min
ggplot(New.Fig) +
  geom_point(aes(D,net.energy.gain_min , color = species), size =3) +
  #geom_text_repel(aes(C, net.energy.gain_min, label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  geom_abline (lty = 3, size = 1, intercept = -55.608, slope = mean(New.Fig$lunge.rate))+
  #geom_smooth(aes(C,net.energy.gain_min),method = "lm", lty = 3, se = FALSE)+
  ylab("Lunge Rate (net kJ/min)")+xlab("Metabolic costs (net kJ/lunge)")

##!!!##
New.Fig<- New.Fig[order(New.Fig$net.energy.gain_min),]
ggplot(New.Fig) +
  geom_point(aes(New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate),1:nrow(New.Fig), color = species), size =3) +
  #geom_text_repel(aes(null, B , label = New.Fig$X, color = species)) +
  theme_classic(base_size = 10)+
  scale_y_continuous(breaks=1:13)+
  geom_vline(xintercept = -250/12.2*60,lty=2)+
  #geom_abline (lty = 3, size = 1, intercept = 0, slope = mean(New.Fig$prop.captured..no.whale.behavior)*24.8*0.84)+
  ylab("ranking")+xlab("change in (kJ/min) when considering lunge rate")+geom_segment(y = 1:nrow(New.Fig), yend = 1:nrow(New.Fig),x = 0,xend =New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate))

ggplot(New.Fig)+
  geom_point(aes(New.Fig$net.energy.gain_min,1, color = species, shape = New.Fig$depth), size =3) +
  scale_y_continuous()+theme_sleek()+theme(text = element_text(size=10))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  ylab("")+xlab("(kJ/min)")


New.Fig$NEG.Sq<- New.Fig$net.energy.gain_min-New.Fig$D*mean(New.Fig$lunge.rate)-55.608
  
SSq.null<-sum(abs(New.Fig$null.Sq))
SSq.B<-sum(abs(New.Fig$B.Sq))
SSq.C<-sum(abs(New.Fig$C.Sq))
SSq.D<-sum(abs(New.Fig$D.Sq))
SSq.NEG<-sum(abs(New.Fig$NEG.Sq))
####

New.Fig2<-melt(New.Fig[,c(1,3,4,14,15,18,20,13)], id=c("X","species","depth") )
New.Fig2$rank<-NA

New.Fig2$variable = factor(New.Fig2$variable,levels(New.Fig2$variable)[c(5,4,3,2,1)])
levels(New.Fig2$variable)<-c("lunge rates","metabolic costs","tactics","prey capture","energy distribution")
for (n in levels(New.Fig2$variable)[4:5]){
  for (x in 1:length(New.Fig2[New.Fig2$variable == n,]$rank)){
    New.Fig2[New.Fig2$variable == n,]$rank[x]<-length(unique(New.Fig2[New.Fig2$variable == n & New.Fig2$value > New.Fig2[New.Fig2$variable == n,]$value[x],]$value))+3
  }
  }
 
for (n in levels(New.Fig2$variable)[1:3]){
  for (x in 1:length(New.Fig2[New.Fig2$variable == n,]$rank)){
    New.Fig2[New.Fig2$variable == n,]$rank<-14-rank(New.Fig2[New.Fig2$variable == n,]$value)
  }
}

S_sqrt <- function(x){sign(x)*(abs(x))^(1/2)}
S_cbrt <- function(x){sign(x)*(abs(x))^(1/10)}
IS_cbrt <- function(x){x^10*sign(x)}
IS_sqrt <- function(x){x^(2)*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
S_cbrt_trans <- function() trans_new("S_cbrt",S_cbrt,IS_cbrt)

ggplot()+geom_point(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)), color = New.Fig2$species))+
  #scale_x_continuous(limits = c(-2000,20000))+
  theme_sleek()+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)),group = New.Fig2$X,color = New.Fig2$species))
 # scale_x_log10() 

ggplot()+ 
  geom_point(aes(New.Fig2$rank,New.Fig2$variable, color = New.Fig2$species), size = 3)+
  #geom_text(aes(New.Fig2[New.Fig2$variable == "tactics",]$rank,New.Fig2[New.Fig2$variable == "tactics",]$variable, label = New.Fig2[New.Fig2$variable == "tactics",]$depth), size = 3)+
  scale_x_reverse()+
  theme_sleek()+
  xlab("rank")+ ylab("model")+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$rank,New.Fig2$variable,group = New.Fig2$X,color = New.Fig2$species))
# scale_x_log10() 

ggplot()+geom_point(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable))))+
  scale_x_reverse()+
  theme_sleek()+
  #scale_x_continuous(trans="S_sqrt")+
  geom_path(aes(New.Fig2$value,-as.numeric(as.factor(New.Fig2$variable)),group = New.Fig2$X,color = New.Fig2$species))
# scale_x_log10() 

>>>>>>> 62287a09f0b5037828084c2240836e89d9240d0e
