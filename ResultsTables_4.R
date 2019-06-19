se <- function(x, na.rm = TRUE) {
  
  sd(x,na.rm = )/sqrt(sum(!is.na(x)))}
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
Table_R1<-EnergyCosts[order(EnergyCosts$group),c("tag", "group","dur","group.size","lunge.depth",
                                                 "dive.depth","max.speed","meta.costs.mean","meta.costs.se",
                                                 "total.meta.cost","lunge_min","meta.costs_min")]
write.csv(Table_R1,"Table_R1.csv")
#results.keep<-results
results<-results.keep
EnergyCosts$group<-as.character(EnergyCosts$group)
EnergyCosts[EnergyCosts$group == "HerringTI_large"|EnergyCosts$group == "HerringTI_small", ]$group<-"HerringTI"
EnergyCosts$group<-as.factor(EnergyCosts$group)
EnergyCosts[EnergyCosts$prey=="spawning herring",]$prey<-"herring"
sum.EnergyCosts<-summaryBy(lunge_min+meta.costs_min+meta.costs.mean~group, FUN= c(mean,se),data = EnergyCosts)
sum.EnergyCosts$group<-as.character(sum.EnergyCosts$group)
#results<- read.csv("C:/Users/Ellen/Desktop/results3.csv")
results$group<-as.character(results$group)
results$group2<-results$group
results$group2<-as.character(results$group2)
results$prey<-NA
results[results$group=="HerringTI" | results$group=="HerringSC"| results$group=="HerringKB", ]$prey<-"Herring"
results[results$group=="KrillSS"| results$group=="KrillSC", ]$prey<-"Krill"
results[ results$group=="ChumKB"| results$group=="ChumTB", ]$prey<-"Chum Salmon"
results[results$group=="CohoAtRelease" | results$group=="CohoAfterRelease", ]$prey<-"Coho Salmon"

results[results$group2 == "CohoAtRelease",]$group2<-"CohoKB"
results[results$group2 == "CohoAfterRelease",]$group2<-"CohoKB"
results[results$group2 == "ChumKB",]$group2<-"ChumTB"

results$lunge_min<-NA
results$lunge_min.se<-NA
results$meta.costs_min<-NA
results$meta.costs_min.se<-NA
results$meta.costs.mean_lunge<-NA
results$meta.costs.mean_lunge.se<-NA
results$net.energy.gain.kJ_lunge<-NA
results$net.energy.gain.kJ_min<-NA
results$lunge.efficiency<-NA
results$group<-as.character(results$group)
results$group2<-as.character(results$group2)
results$group<-as.character(results$group)
results[10,]$group<-"Swimming"
results$group<-as.factor(results$group)
results[10,]$gross.energy_gainkJ.Lunge.mean<-0
results[10,]$meta.costs.mean_lunge<-3*BMR*12.2
results[10,]$net.energy.gain.kJ_lunge<-(-3*BMR*12.2)
results[10,]$net.energy.gain.kJ_min<-(-3*BMR*60)
results[10,]$.energy_m3.mean<-0
results[10,]$.prop_captured.mean<-0
results[10,]$prey<-"None"
  
for (n in which(results$group2!="HerringKB")){
  results[n,]$lunge_min<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$lunge_min.mean
  results[n,]$lunge_min.se<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$lunge_min.se
  results[n,]$meta.costs_min<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$meta.costs_min.mean
  results[n,]$meta.costs_min.se<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$meta.costs_min.se
  results[n,]$meta.costs.mean_lunge<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$meta.costs.mean.mean
  results[n,]$meta.costs.mean_lunge.se<-sum.EnergyCosts[sum.EnergyCosts$group==results[n,]$group2,]$meta.costs.mean.se
  results[n,]$net.energy.gain.kJ_lunge<-results[n,]$gross.energy_gainkJ.Lunge.mean-results[n,]$meta.costs.mean_lunge
  results[n,]$net.energy.gain.kJ_min<-results[n,]$gross.energy_gainkJ.Lunge.mean*results[n,]$lunge_min-results[n,]$meta.costs_min
  results[n,]$lunge.efficiency<-results[n,]$gross.energy_gainkJ.Lunge.mean/results[n,]$meta.costs.mean_lunge
}
results[results$prey=="Chum Salmon" | results$prey=="Coho Salmon",]$meta.costs.mean_lunge.se<-EnergyCosts[EnergyCosts$group=="ChumTB",]$meta.costs.se
n<-which(results$group2=="HerringKB")
results[n,]$lunge_min<-mean(EnergyCosts[EnergyCosts$prey=="herring",]$lunge_min)
results[n,]$lunge_min.se<-se(EnergyCosts[EnergyCosts$prey=="herring",]$lunge_min)
results[n,]$meta.costs_min<-mean(EnergyCosts[EnergyCosts$prey=="herring",]$meta.costs_min)
results[n,]$meta.costs_min.se<-se(EnergyCosts[EnergyCosts$prey=="herring",]$meta.costs_min)
results[n,]$meta.costs.mean_lunge<-mean(EnergyCosts[EnergyCosts$prey=="herring",]$meta.costs.mean)
results[n,]$meta.costs.mean_lunge.se<-se(EnergyCosts[EnergyCosts$prey=="herring",]$meta.costs.mean)
results[n,]$net.energy.gain.kJ_lunge<-results[n,]$gross.energy_gainkJ.Lunge.mean-results[n,]$meta.costs.mean_lunge
results[n,]$net.energy.gain.kJ_min<-results[n,]$gross.energy_gainkJ.Lunge.mean*results[n,]$lunge_min-results[n,]$meta.costs_min
results[n,]$lunge.efficiency<-results[n,]$gross.energy_gainkJ.Lunge.mean/results[n,]$meta.costs.mean_lunge
results$group<-as.factor(results$group)
results$group = factor(results$group,results[order(results$net.energy.gain.kJ_min),]$group)
textsize <-12
results$group3<-results$group
results$group<-as.character(results$group)
results[4,]$group<- "ChumKB/TB Late"
results[5,]$group<- "ChumKB Early"
results[6,]$group<- "Coho At Release"
results[8,]$group<- "Coho After Release"

p1 <- ggplot(data = results, aes(x = reorder(group, .energy_m3.mean), y = .energy_m3.mean, fill = prey))+
  geom_bar(stat = "identity")+
geom_errorbar(aes(x = reorder(group, .energy_m3.mean), ymin = .energy_m3.mean-.energy_m3.se, ymax = .energy_m3.mean+.energy_m3.se), width = 0.25)+ylab(expression("Patch energy density (kJ/m"^3*")"))+
theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+
  theme(panel.border = element_blank(),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))

p2 <- ggplot()+geom_bar(aes(x = reorder(group, .prop_captured.mean), y = .prop_captured.mean, fill = prey), data = results,stat = "identity")+ylab("Proportin captured")+theme_classic()+xlab("") +theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
p3 <- ggplot()+geom_bar(aes(x = reorder(group,lunge.efficiency), y = lunge.efficiency, fill = prey), data = results,stat = "identity")+ylab("Lunge efficiency")+theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+geom_hline(aes(yintercept = 1), linetype = "dotted")
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
  p3.1 <- ggplot()+geom_bar(aes(x = reorder(group,-meta.costs.mean_lunge), y = meta.costs.mean_lunge, fill = prey), data = results,stat = "identity")+ylab("Metabolic Costs (kJ)")+theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+geom_hline(aes(yintercept = 1), linetype = "dotted")+
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
  
  
p4 <- ggplot()+geom_bar(aes(x = reorder(group, net.energy.gain.kJ_lunge), y = net.energy.gain.kJ_lunge, fill = prey), data = results,stat = "identity")+ylab("Net energy gain (kJ/lunge)")+theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+geom_hline(aes(yintercept = 0), linetype = "dotted")+
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
p5 <- ggplot()+geom_bar(aes(x = reorder(group, net.energy.gain.kJ_min),y = net.energy.gain.kJ_min, fill = prey), data = results,stat = "identity")+ylab("Net energy gain (kJ/min)")+theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+geom_hline(aes(yintercept = 0), linetype = "dotted")+
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
p6 <- ggplot()+geom_bar(aes(x = reorder(group, lunge_min),y = lunge_min, fill = prey), data = results,stat = "identity")+ylab("Net energy gain (kJ/min)")+theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange","black"))+geom_hline(aes(yintercept = 0), linetype = "dotted")+
  theme(panel.border = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=25, hjust=1),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.background = element_blank(),text = element_text(size=textsize))
multiplot(p1,p2,p3.1,p4,p5)
ggsave("p1.tiff",p1, path = "C:/Users/Ellen/Desktop", width = 7, height = 3, units = c("in"),
       dpi = 300)
ggsave("p2.tiff",p2, path = "C:/Users/Ellen/Desktop" , width = 7, height = 3, units = c("in"),
       dpi = 300)
ggsave("p3.tiff",p3.1, path = "C:/Users/Ellen/Desktop" , width = 7, height = 3, units = c("in"),
       dpi = 300)
ggsave("p4.tiff",p4, path = "C:/Users/Ellen/Desktop" , width = 7, height = 3, units = c("in"),
       dpi = 300)
ggsave("p5.tiff",p5, path = "C:/Users/Ellen/Desktop" , width = 7, height = 3, units = c("in"),
       dpi = 300)

p6 <- ggplot(data = results, aes(x = reorder(group, lunge_min), y = lunge_min, fill = prey))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = reorder(group, lunge_min), ymin = lunge_min-lunge_min.se, ymax = lunge_min+lunge_min.se), width = 0.25)+ylab("lunge rate (lunges/min)")+
  theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange"))
p7 <- ggplot(data = results, aes(x = reorder(group, net.energy.gain.kJ_lunge), y = -meta.costs.mean_lunge, fill = prey))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = reorder(group, -meta.costs.mean_lunge), ymin = -meta.costs.mean_lunge-meta.costs.mean_lunge.se, ymax = -meta.costs.mean_lunge+meta.costs.mean_lunge.se), width = 0.25)+ylab("meta cost per lunge(kJ/lunge)")+
  theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange"))#+
  #scale_y_continuous(limits = c(-32500,0))
p8 <- ggplot(data = results, aes(x = reorder(group, net.energy.gain.kJ_lunge), y = gross.energy_gainkJ.Lunge.mean, fill = prey))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = reorder(group, gross.energy_gainkJ.Lunge.mean), ymin = gross.energy_gainkJ.Lunge.mean-.energy_gain.se, ymax = gross.energy_gainkJ.Lunge.mean+.energy_gain.se), width = 0.25)+ylab("energy gain (kJ/lunge)")+
  theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange"))#+
  #scale_y_continuous(limits = c(0, 32500))
p9 <- ggplot(data = results, aes(x = reorder(group, net.energy.gain.kJ_lunge), y = -meta.costs.mean_lunge, fill = prey))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = reorder(group, -meta.costs.mean_lunge), ymin = -meta.costs.mean_lunge-meta.costs.mean_lunge.se, ymax = -meta.costs.mean_lunge+meta.costs.mean_lunge.se), width = 0.25)+ylab("meta cost per lunge(kJ/lunge)")+
  theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.text.x=element_text(angle=25, hjust=1))+
  scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange"))+
scale_y_continuous(limits = c(-32500,0))
p10 <- ggplot(data = results, aes(x = reorder(group, net.energy.gain.kJ_lunge), y = gross.energy_gainkJ.Lunge.mean, fill = prey))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = reorder(group, gross.energy_gainkJ.Lunge.mean), ymin = gross.energy_gainkJ.Lunge.mean-.energy_gain.se, ymax = gross.energy_gainkJ.Lunge.mean+.energy_gain.se), width = 0.25)+ylab("energy gain (kJ/lunge)")+
  theme_classic() +xlab("")+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  scale_fill_manual(values=c("yellow", "grey", "dodgerblue2","orange"))+
scale_y_continuous(limits = c(0, 32500))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
multiplot(p8,p7)# costs and bennies on their own scales
multiplot(p10,p9) #costs and bennies on a common scale

#effect of whale length on lunge efficiency+net energy gain
{
  prop_captured.vec<-c()
  for (s in c(1,3,6)){
    var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
    var$values<-var[,6+s]
meta.costs_lunge<-((0.5*mass*(20.25)+2.4)/1000)/0.16 #maxV2-minV2 based on Sitka Sound 
assignVec(as.character(var$Rname),var$values3, envir = .GlobalEnv) 
var$values<-var$values4
KrillSS.10<-Energy_gain.nodf(Vp = 0.14, Vp.sd = 0.05,
                          Ddetect = 0.9, Ddetect.sd = 0.1, 
                          lungetype = "subsurface", 
                          assim_eff = 0.92, 
                          engulf_vol_m3 = 42,# will alter this later
                          mass_ind = EnergyParameters[4,]$Wet_mass_g,
                          energy_g = EnergyParameters[4,]$Energy_kJ.g,
                          Sv_mean = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")$Sv_mean,
                          TS = -85,#Cade estimate.  Needs to be addressed
                          var = var,
                          Prey = "krill",
                          Data = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv"),
                          Vc_0 = 4.6,#cade for krill
                          proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)
prop_captured.vec<-c(prop_captured.vec,prop_captured)}
length2<-c(10,11.1,12.3,13.3,14,14.4)
engulf_vol<-(8.0094*length2 ^3.21)/1020 # equation from GOldbogen et al 2012 for kg.  / 1020 to accoutn for weight of seawater
plot(length2,prop_captured)

p11<-ggplot()+geom_smooth(aes(length2, prop_captured), color = 1,se = FALSE)+ylab("proportion captured")+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                   axis.text.x=element_blank(),
                                                                                                                   axis.ticks.x=element_blank())
p12<-ggplot()+geom_smooth(aes(length2, meta.costs_lunge),se = FALSE)+ylab("metabolic costs")+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                  axis.text.x=element_blank(),
                                                                                                                  axis.ticks.x=element_blank())
p13<-ggplot()+geom_smooth(aes(length2, prop_captured*259.01840*engulf_vol*0.92),se = FALSE)+ylab("gross energy gain (kJ)")+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                                axis.text.x=element_blank(),
                                                                                                                                axis.ticks.x=element_blank())
p14<-ggplot()+geom_smooth(aes(length2, prop_captured*259.01840*engulf_vol*0.92-meta.costs_lunge),se = FALSE)+ylab("net energy gain (kJ/lunge)")+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                                                     axis.text.x=element_blank(),
                                                                                                                                                     axis.ticks.x=element_blank())
p15<-ggplot()+geom_smooth(aes(length2, (prop_captured*259.01840*engulf_vol*0.92)/meta.costs_lunge),color =1,se = FALSE)+ylab("lunge efficiency")+theme_classic()+xlab("Whale length")

p16<-ggplot()+geom_smooth(aes(length2, meta.costs_lunge),color = 2,se = FALSE)+
  geom_smooth(aes(length2, prop_captured*259.01840*engulf_vol*0.92),se = FALSE)+
  geom_smooth(aes(length2, prop_captured*259.01840*engulf_vol*0.92-meta.costs_lunge),color =1,se = FALSE)+
  ylab("KJ/lunge")+theme_classic()+theme(axis.title.x=element_blank(),
                                                        axis.text.x=element_blank(),
                                                        axis.ticks.x=element_blank())

multiplot(p11,p16,p15)

mod<-lm(prop_captured~poly(length2,2))
}

#effect of size and speed on lunge efficiency
{
  #b<-4
  b<-5
Vc_0.vec<-c(1,2,3,4,5,6)
Vmin.vec<-c()
var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
prop_captured.vec<-c()
dataframe<-c()
for (s in c(1,3,6)){
  var$values<-var[,6+s]
for (o in Vc_0.vec){
  
  KRILL<-Energy_gain.nodf(Vp = EnergyParameters[b,]$Prey.speed,
                          Vp.sd = EnergyParameters[b,]$SD.Prey.speed,
                          Ddetect = EnergyParameters[b,]$Mean.Reaction.distance,
                          Ddetect.sd = EnergyParameters[b,]$SD.Reaction.distance,
                          mass_ind = EnergyParameters[b,]$Mean.Wet.mass,
                            energy_g = EnergyParameters[b,]$Energy,
                            Sv_mean = get(EnergyParameters[b,]$Sv)$Sv_mean,
                            proportion_captured = "formula",
                            TS = EnergyParameters[b,]$TS,
                            var = var,
                            lungetype = "subsurface",
                            Prey = EnergyParameters[b,]$Prey,
                            Data = get(EnergyParameters[b,]$Sv),
                            Vc_0 = o,
                          grid = 15) # insert value or specify "formula" to calcualte based on inputs)
  print(prop_captured)
  prop_captured.vec[length(prop_captured.vec)+1] <- prop_captured
Vmin.vec[length(Vmin.vec)+1] = min(df$Vc.t)
meta.costs_lunge<-((0.5*Mc*(Vc_0.vec^2-Vmin.vec^2)+Mc*2.4)/1000)/0.16

}
  dataframe<-rbind(dataframe, cbind(prop_captured.vec,Lbody,meta.costs_lunge,Vc_0.vec,Vmin.vec))
}
dataframe.keep2<-dataframe
write.csv(dataframe.keep, "C:/Users/Ellen/Desktop/Analysis/Chapter3/SpeedSizeData2.csv")
dataframe<-as.data.frame(dataframe)
dataframe$engulfment.vol<-(8.0094*dataframe$Lbody ^3.21)/1020
dataframe$net.energy.gain<-dataframe$prop_captured.vec*321*dataframe$engulfment.vol*0.84-dataframe$meta.costs_lunge
 
 #maxV2-minV2 based on Sitka Sound 

p13<-ggplot()+geom_smooth(aes(Vc_0.vec, prop_captured.vec, size = as.factor(Lbody), linetype = as.factor(Lbody)), data = dataframe, color = 1,se = FALSE)+
  ylab("proportion captured")+scale_size_manual(values = c(0.5,1,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                               axis.text.x=element_blank(),
                                                                                                                         axis.ticks.x=element_blank())
p16<-ggplot()+geom_smooth(aes(Vc_0.vec, meta.costs_lunge,  size = as.factor(Lbody),linetype = as.factor(Lbody)), data = dataframe,color = 2,se = FALSE)+
  geom_smooth(aes(Vc_0.vec, prop_captured.vec*3330*engulfment.vol*0.84,  size = as.factor(Lbody),linetype = as.factor(Lbody)), data = dataframe,se = FALSE)+
  geom_smooth(aes(Vc_0.vec, prop_captured.vec*3330*engulfment.vol*0.84-meta.costs_lunge, size = as.factor(Lbody), linetype = as.factor(Lbody)), data = dataframe,color =1,se = FALSE)+
  
  ylab("KJ/lunge")+theme_classic()+theme(axis.title.x=element_blank(),
                                         axis.text.x=element_blank(),
                                         axis.ticks.x=element_blank())+scale_size_manual(values = c(0.5,1.5,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))
p16.2<-ggplot()+geom_smooth(aes(Vc_0.vec, meta.costs_lunge), size = 2, data = dataframe,color = 2,se = FALSE)+
  geom_smooth(aes(Vc_0.vec, prop_captured.vec*3330*engulfment.vol*0.84  ), size = 2, data = dataframe,se = FALSE)+
  geom_smooth(aes(Vc_0.vec, prop_captured.vec*3330*engulfment.vol*0.84-meta.costs_lunge), size = 2, data = dataframe,color =1,se = FALSE)+
  
  ylab("KJ/lunge")+theme_classic()

p15<-ggplot()+geom_smooth(aes(Vc_0.vec, (prop_captured.vec*3330*engulfment.vol*0.84)/meta.costs_lunge, linetype = as.factor(Lbody), size = as.factor(Lbody)),data = dataframe, color =1,se = FALSE)+
  ylab("lunge efficiency")+theme_classic()+xlab("Initial speed (m/s)")+scale_size_manual(values = c(0.5,1,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))

multiplot(p13,p16,p15)
}

#effect of size and speed on lunge efficiency (skinny whales)
{
  Vc_0.vec<-c(1.389,2.198,3.089,3.443,4.299,6)
  Vmin.vec<-c()
  var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
  
  prop_captured.vec<-c()
  dataframe<-c()
  for (s in c(1,3,6)){
    var$values<-var[,6+s]
    var[13,]$values<- 10^(log10((0.0158*var[9,]$values^2.95)*1000)-0.083)
    for (o in Vc_0.vec){
      
      KrillSS<-Energy_gain.nodf(Vp = 0.14, 
                                Ddetect = 0.9, 
                                lungetype = "subsurface", 
                                assim_eff = 0.92, 
                                engulf_vol_m3 = 42,
                                mass_ind = EnergyParameters[4,]$Wet_mass_g,
                                energy_g = EnergyParameters[4,]$Energy_kJ.g,
                                Sv_mean = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")$Sv_mean,
                                TS = -85,#Cade estimate.  Needs to be addressed
                                var = var,
                                Prey = "krill",
                                Data = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv"),
                                Vc_0 = o,#cade for krill
                                proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)
      prop_captured.vec[length(prop_captured.vec)+1] = prop_captured
      Vmin.vec[length(Vmin.vec)+1] = min(df$Vc.t)
      meta.costs_lunge<-((0.5*Mc*(Vc_0.vec^2-Vmin.vec^2)+Mc*2.4)/1000)/0.16
      
    }
    dataframe<-rbind(dataframe, cbind(prop_captured.vec,Lbody,meta.costs_lunge,Vc_0.vec,Vmin.vec))
  }
  dataframe<-as.data.frame(dataframe)
  dataframe$engulfment.vol<-(8.0094*dataframe$Lbody ^3.21)/1020
  dataframe$net.energy.gain<-dataframe$prop_captured.vec*259.01840*dataframe$engulfment.vol*0.94-dataframe$meta.costs_lunge
  dataframe<-
    dataframe<-
    #maxV2-minV2 based on Sitka Sound 
    
    p13<-ggplot()+geom_smooth(aes(Vc_0.vec, prop_captured.vec, size = as.factor(Lbody), linetype = as.factor(Lbody)), data = dataframe, color = 1,se = FALSE)+
    ylab("proportion captured")+scale_size_manual(values = c(0.5,1,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                                                                                            axis.text.x=element_blank(),
                                                                                                                                                            axis.ticks.x=element_blank())
  p16<-ggplot()+geom_smooth(aes(Vc_0.vec, meta.costs_lunge,  size = as.factor(Lbody),linetype = as.factor(Lbody)), data = dataframe,color = 2,se = FALSE)+
    geom_smooth(aes(Vc_0.vec, prop_captured.vec*259.01840*engulfment.vol*0.94,  size = as.factor(Lbody),linetype = as.factor(Lbody)), data = dataframe,se = FALSE)+
    geom_smooth(aes(Vc_0.vec, prop_captured.vec*259.01840*engulfment.vol*0.94-meta.costs_lunge, size = as.factor(Lbody), linetype = as.factor(Lbody)), data = dataframe,color =1,se = FALSE)+
    
    ylab("KJ/lunge")+theme_classic()+theme(axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank())+scale_size_manual(values = c(0.5,1.5,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))
  
  p15<-ggplot()+geom_smooth(aes(Vc_0.vec, (prop_captured.vec*259.01840*42*0.94)/meta.costs_lunge, linetype = as.factor(Lbody), size = as.factor(Lbody)),data = dataframe, color =1,se = FALSE)+
    ylab("lunge efficiency")+theme_classic()+xlab("Initial speed (m/s)")+scale_size_manual(values = c(0.5,1,0.5))+scale_linetype_manual(values = c("dotted","solid","dashed"))
  
  multiplot(p13,p16,p15)
}
####  sensitivity to prey speed and Rxn distance
{
  var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
  b<-5
prop_captured.vec<-c()
dataframe<-c()
Rxn.vec<-c(0.25,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
speed.vec<-c(0.05,0.10,0.25,0.5,1,1.25,1.5,2,2.5,3)
df<-Create.df(var = var, Prey = EnergyParameters[b,]$Prey, Vc_0 = 4.1)

# 2nd half Rxn.vec<-c(0.5,2,3.5,5)
# 2nd half speed.vec<-c(0.1,0.5,1.25,2,3)
for(Rxn in Rxn.vec){
  for (speed in speed.vec){
  
  Capture.prop.stochastic(Vp=speed, 
                          Ddetect=Rxn, 
                          Vp.sd=speed*0.0374,
                          Ddetect.sd=Rxn*0.66, 
                          df=df, 
                          lungetype="subsurface", 
                          var=var, 
                          grid = 15)
  
   prop_captured.vec[length(prop_captured.vec)+1] <- prop_captured
}
  dataframe<-rbind(dataframe,cbind(prop_captured.vec,speed.vec,Rxn))
  prop_captured.vec<-c()
}
}
dataframe.keep.6<-dataframe
dataframe.5<-read.csv("contourplot2_data4.csv")
dataframe<-dataframe.6
dataframe<-as.data.frame(dataframe)
PreyEscapeParameters<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/Chapter3ModParValues.csv")
names(dataframe)<-c("prop_captured.vec","speed","Rxn")
ggplot()+geom_point(aes(x = Rxn, y = speed, size = prop_captured.vec), data = dataframe)
ggplot()+geom_contour(aes(x = Rxn, y = speed, z = prop_captured.vec),size=1, data = dataframe)+
  ylab("Prey speed (m/s)")+xlab("Prey reaction distance (m)")+
  theme(text = element_text(size=40))+
  #geom_point(aes(x = Rxn, y = speed,color = prop_captured.vec),size=1, data = dataframe)+ylab("Prey speed (m/s)")+xlab("Prey reaction distance (m)")+
  #geom_text(aes(label=round(prop_captured,2), z=NULL), data=dataframe)+
  #geom_point(aes(x=Rxn, y=speed), data = PreyEscapeParameters)+
  geom_point(aes(x=Mean.Reaction.distance, y = Prey.speed, colour = Proportion_surface),data = PreyEscapeParameters)+scale_colour_gradient(low = "black", high = "grey")+
  geom_text_repel(aes(x=Mean.Reaction.distance, y = Prey.speed, label = Prey.Group, colour = Proportion_surface), size = 5,data = PreyEscapeParameters)+theme_classic()

##Appendix surface feeding
ggplot()+geom_smooth(aes(p,max.speed),data = AllLunges) +
  geom_point(aes(p,max.speed), data = AllLunges)+
  xlab("Depth (m)")+ylab("Initial whale speed (m/s)")+
  theme_classic()+geom_vline(xintercept = 25, linetype = 2)

ggplot()+
  #geom_text(aes(label=round(prop_captured,2), z=NULL), data=dataframe)+
  #geom_point(aes(x=Rxn, y=speed), data = PreyEscapeParameters)+
  geom_text(aes(x=Rxn, y=speed, label = X, position ="dodge"),data = PreyEscapeParameters)+theme_classic()
meta.costs_lunge<-((0.5*Mc*20.24+2.4)/1000)/0.16
  
mod<-lm(prop_captured.vec~poly(Rxn, 2)*poly(speed,2), data = dataframe)

#######

PropCap<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/PropCapSensAnaly.csv")
PropCap$lunge.type<-as.character(PropCap$lunge.type)
for(z in 1:nrow(PropCap)){
  var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
  var$values<-var[,6+PropCap$s[z]]
  Create.df(var = var,Prey = PropCap$Prey[z], Vc_0 = PropCap$Vc[z])
  Capture.prop.stochastic(Vp = PropCap$Vp[z],
                          Vp.sd = PropCap$Vp.sd[z],
                          Ddetect = PropCap$Rxn[z],
                          Ddetect.sd = PropCap$Rxn.sd[z],
                          df = df,
                          lungetype=PropCap$lunge.type[z],
                          Tclose=Tclose,
                          var = var)
  
  PropCap$prop_captured[z]<-prop_captured
}
{
  #b<-4
  a<-1
  Vc_0.vec<-c(1,2,3,4,5,6)
  v.vec<-c(0.03,0.2,0.6,0.8,1.3)
  v.vec<-c(0.2,0.8)
  # vs.vec<-c(0.01,0.2,0.4,0.6,0.8)
  vs.vec<-0.6
    #d.vec<-c(0.5,1.25, 2.50,3.75, 4.5)
  d.vec<-c(1.25,3.75)
    #ds.vec<-c(0.01,0.05,0.1,0.3,0.6,1)
  ds.vec<-0.05
  Vmin.vec<-c()
  var<-read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv")
  prop_captured.vec<-c()
  dataframe2<-c()
  for(l in c("lateral","subsurface")){
  for (s in c(1,3,6)){
    var$values<-var[,6+s]
    for (o in Vc_0.vec[c(2:6)]){
      Create.df(var = var,Prey = EnergyParameters[a,]$Prey, Vc_0 = o)
      for (v in v.vec) {
        for (vs in vs.vec){
          for (d in d.vec) {
            for (ds in ds.vec){
              #var$values<-as.numeric(as.character(var$values))
              #var$Rname<-as.character(var$Rname)
              #assignVec(var$Rname,var$values, envir = .GlobalEnv) 
      Capture.prop.stochastic(Vp = v,
                              Vp.sd = vs,
                              Ddetect = d,
                              Ddetect.sd = ds,
                              df = df,
                              lungetype=l,
                              Tclose=Tclose,
                              var = var)
         prop_captured.vec[length(prop_captured.vec)+1] <- prop_captured
         dataframe2<-rbind(dataframe2,c(prop_captured,Lbody,o,v,vs,d,ds,l))
#dataframe<-rbind(dataframe, cbind(prop_captured.vec,Lbody,meta.costs_lunge,Vc_0.vec,Vmin.vec))
            }
          }
        }
      }
    }
  }
    }
}
  }
dataframe<-cbind(1:150,prop_captured.vec,10,1,rep(vs.vec, each = 30),rep(rep(d, each = 6),5),ds.vec)
dataframe2<-read.csv("C:/Users/Ellen/Desktop/dataframe.csv")
dataframe2$log.dist<-log(dataframe2$dist)
dataframe2$log.var<-log(dataframe2$var)
dataframe2$var.2<-(dataframe2$var)^2
mod<-lm(prop_captured.vec~var*dist+var+log.dist+poly(rxn,3), data =dataframe2)

mod<-lm(prop_captured.vec~poly(var,2)*dist+log.dist, data =dataframe2)

mod<-lm(prop_captured.vec~poly(var.2,3)*log.dist+log.dist, data =dataframe2)

summary(mod)
ggplot()+geom_smooth(aes(ds.vec,prop_captured.vec, color = as.factor(rxn)), data = dataframe2)

