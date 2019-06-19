#See Potvin et al 2012 in PLOSone Figure 3
#libraryHeight
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("plyr", lib.loc="~/R/win-library/3.4")
library("doBy", lib.loc="~/R/win-library/3.4")
library("directlabels")
##functions##
assignVec <- Vectorize("assign",c("x","value"))
deg2rad <- function(deg) {(deg * pi) / (180)}
se <- function(x, na.rm = TRUE) {
  
  
  sd(x,na.rm = )/sqrt(sum(!is.na(x)))}
#original functions
Create.df<- function(var,Prey, Vc_0){
  #var: dataframe of whale morpholoigcal parameters allometrically scaled
  #Prey: catagorical variable options are 2: "krill" or "fish"
  #Vc_0: initial speed of the whale
  
  #internal functions
  mouthopening<-function(){
    for (t in seq(0.01, Gapmax.t, dt)) {
      #THETAgape.t<- Hsync1*Vc_0/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*t #gape angle at time t if opening EQ6
      THETAgape.t<-(THETAgape/Topen)*t
      .THETAgape.t<-(THETAgape/Topen)
      ifelse(t<=0.66*Topen, PHE<-1.6, PHE<-0 )
      Ac.t<-0.5*pi*Whead/2*(Xjd*Ljaw*sin(deg2rad(THETAgape.t))) # EQ11 mouth area in meters cubed (squared?)
      .Mw.t<-Dwater*Ac.t*(Vc.t-Vw.t)*PHE
      Mw.t<-Mw.t+.Mw.t*dt
      Fbc.t<-Kopen*4*Ac.t*Mw.t/(pi*Whead*Tengulf^2)# EQ16 Force applied to engulfed mass by buccal cavity walls
      Fed.t<-Fbc.t
      Fsd.t<-(Cd_open*Ac.t+Cd_body*Abody)*0.5*Dwater*Vc.t^2 #EQ20
      Fextthrust<- 4000*(Lbody-0.77)/(14.40-0.77)*(Lbody/14.40)^0.82 #Thrust plus bouyancy
      
      
      ac.t<-(Fextthrust-Fed.t-Fsd.t)/Mc
      Fww<-0
      aw.t<- (Fbc.t-Fww-Vw.t*.Mw.t)/Mw.t
      Vw.t<-Vw.t+aw.t*dt
      Vc.t<- Vc.t+ac.t*dt
      
      df[n,1]<-t # time
      df[n,2]<-Ac.t #Area of mouth at time = t
      df[n,3]<-Mw.t #Mass of engulfed water at time = t
      df[n,4]<-.Mw.t # filling rate of the mouth at time t
      df[n,5]<-Vc.t # speed of the whale at tiem t
      df[n,6]<-Vw.t # speed of the water at time t
      df[n,7]<-ac.t #acceleration of whale at time t
      df[n,8]<-aw.t # acceleration of water at time t
      df[n,9]<-THETAgape.t # angle of jaw at time t
      df[n,10]<-Fed.t
      df[n,11]<-Fsd.t
      df[n,12]<-Fextthrust
      df[n,13]<-.THETAgape.t
      n<-n+1
    }
    assign("df",df, envir = .GlobalEnv)
    assign("n",n, envir = .GlobalEnv)
    assign("Vc.t",Vc.t, envir = .GlobalEnv)
    assign("Vw.t",Vw.t, envir = .GlobalEnv)
    assign("ac.t",ac.t, envir = .GlobalEnv)
    assign("aw.t",aw.t, envir = .GlobalEnv)
    assign("Ac.t",Ac.t, envir = .GlobalEnv)
    assign("Mw.t",Mw.t, envir = .GlobalEnv)
    assign(".Mw.t",.Mw.t, envir = .GlobalEnv)
    assign("THETAgape.t",THETAgape.t, envir = .GlobalEnv)
    assign(".THETAgape.t",THETAgape.t, envir = .GlobalEnv)
    assign("Fed.t",Fed.t, envir = .GlobalEnv)
    assign("Fsd.t",Fsd.t, envir = .GlobalEnv)
    assign("Fextthrust",Fextthrust, envir = .GlobalEnv)
    
  }
  mouthopen<-function(){
    for (t in seq(Gapmax.t+dt,Gapmax.t+GapemaxDur.mean , dt)) {
      THETAgape.t<-THETAgape #gape angle at time t if opening EQ6
      PHE<-0 
      Ac.t<-0.5*pi*Whead/2*(Xjd*Ljaw*sin(deg2rad(THETAgape.t))) # EQ11 mouth area in meters cubed (squared?)
      .Mw.t<-Dwater*Ac.t*(Vc.t-Vw.t)*PHE
      Mw.t<-Mw.t+.Mw.t*dt
      Fbc.t<-Kopen*4*Ac.t*Mw.t/(pi*Whead*Tengulf^2)# EQ16 Force applied to engulfed mass by buccal cavity walls
      Fed.t<-Fbc.t
      Fsd.t<-(Cd_open*Ac.t+Cd_body*Abody)*0.5*Dwater*Vc.t^2 #EQ20
      Fextthrust<- 4000*(Lbody-0.77)/(14.40-0.77)*(Lbody/14.40)^0.82 #Thrust plus bouyancy
      
      ac.t<-(Fextthrust-Fed.t-Fsd.t)/Mc
      Fww<-0
      aw.t<- (Fbc.t-Fww-Vw.t*.Mw.t)/Mw.t
      Vw.t<-Vw.t+aw.t*dt
      Vc.t<- Vc.t+ac.t*dt
      
      df[n,1]<-t # time
      df[n,2]<-Ac.t #Area of mouth at time = t
      df[n,3]<-Mw.t #Mass of engulfed water at time = t
      df[n,4]<-.Mw.t # filling rate of the mouth at time t
      df[n,5]<-Vc.t # speed of the whale at tiem t
      df[n,6]<-Vw.t # speed of the water at time t
      df[n,7]<-ac.t #acceleration of whale at time t
      df[n,8]<-aw.t # acceleration of water at time t
      df[n,9]<-THETAgape.t # angle of jaw at time t
      df[n,10]<-Fed.t
      df[n,11]<-Fsd.t
      df[n,12]<-Fextthrust
      n<-n+1
    }
    assign("df",df,envir = .GlobalEnv)
    assign("n",n, envir = .GlobalEnv)
    assign("Vc.t",Vc.t, envir = .GlobalEnv)
    assign("Vw.t",Vw.t, envir = .GlobalEnv)
    assign("ac.t",ac.t, envir = .GlobalEnv)
    assign("aw.t",aw.t, envir = .GlobalEnv)
    assign("Fed.t",Fed.t, envir = .GlobalEnv)
    assign("Fsd.t",Fsd.t, envir = .GlobalEnv)
    assign("Fextthrust",Fextthrust, envir = .GlobalEnv)
    assign("Mw.t",Mw.t, envir = .GlobalEnv)
    assign(".Mw.t",.Mw.t, envir = .GlobalEnv)
    assign("Ac.t",Ac.t, envir = .GlobalEnv)
    assign("THETAgape.t",THETAgape.t, envir = .GlobalEnv)
  }
  emptydf<-function(){
    assign("Kclose",Kopen/(Kopen__Kclose),envir = .GlobalEnv)
    assign("Vc.t",Vc_0,envir = .GlobalEnv)
    t<-dt
    assign("Vw.t",0,envir = .GlobalEnv)
    assign("Fww",0,envir = .GlobalEnv)#  "Ocean to engulfed mass" drag
    assign("Mw.t",0,envir = .GlobalEnv)
    assign("Ac.t",0,envir = .GlobalEnv)
    THETAgape.t<-0
    df <- data.frame(t=numeric(1),
                     Ac.t=numeric(1), 
                     Mw.t=numeric(1), 
                     .Mw.t=numeric(1),
                     Vc.t=numeric(1),
                     Vw.t=numeric(1),
                     ac.t=numeric(1),
                     aw.t=numeric(1),
                     THETAgape.t= numeric(1),
                     Fed.t= numeric(1),
                     Fsd.t= numeric(1),
                     Fextthrust.t = numeric(1))
    df[1,1]<-0
    df[1,2]<-Ac.t
    df[1,3]<-Mw.t
    df[1,4]<-0
    df[1,5]<-Vc.t
    df[1,6]<-Vw.t
    df[1,7]<-0
    df[1,8]<-0
    df[1,9]<-THETAgape.t
    assign("df",df, envir = .GlobalEnv)
    assign("n",2, envir = .GlobalEnv)
    assign("t",dt, envir = .GlobalEnv)
    assign("THETAgape.t",THETAgape.t, envir = .GlobalEnv)
    #Hsync<-(Xjd/Topen)*(Ljaw/Vc_0)*(sin(deg2rad(THETAgape))*THETAgape)
  }
  
  #import values
  var$values<-as.numeric(as.character(var$values))
  var$Rname<-as.character(var$Rname)
  assignVec(var$Rname,var$values, envir = .GlobalEnv)  
  
  #Calculate engulf duration (equ 8)and total engulfment capacity (equn 9 and 10)
  Tengulf<- (1/(THETAgape*sin(deg2rad(THETAgape)))*Ljaw/(L0-Ljaw)*Ljaw/Vc_0*sin(deg2rad(THETAgape))*THETAgape)*2  #EQ 8 with substitution for Hsync
  Mw_postTMJ<-Dwater*pi/3*(L0-Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 9 engulfed mass post TMJ
  Mw_antTMJ<-Dwater*pi/3*(Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 10
  Mw<-Mw_postTMJ+Mw_antTMJ
  
  if (Prey == "krill"){
    Gapmax.t<-Tengulf*0.35
    GapemaxDur.mean<-Tengulf*0.25
  }
  if (Prey == "fish"){
    Gapmax.t<-Tengulf*0.25
    GapemaxDur.mean<-Tengulf*0.44
  }
  
  Topen<-Gapmax.t
  #Hsync1<- (Xjd/Topen)*(Ljaw/Vc_0)*sin(deg2rad(THETAgape))*THETAgape
  
  assign("Tclose",GapemaxDur.mean+Gapmax.t, envir = .GlobalEnv)
  #Hsync2<- (Xjd/Tclose)*(Ljaw/Vc_0)*sin(deg2rad(THETAgape))*THETAgape
  
  Kopen<-10.7
  emptydf()
  mouthopening()
  mouthopen()
  while (Vc.t-Vw.t > 0.0001|Vc.t-Vw.t < -0.0001 ){
    #ifelse(Vc.t-Vw.t > 0.0001,Kopen<- Kopen+0.0001,Kopen<- Kopen-0.0001)
    Kopen<- Kopen+(Vc.t-Vw.t)*0.1
    emptydf()
    mouthopening()
    mouthopen()
    print(Vc.t-Vw.t)
  }
  
  #Mouthclosing stage
  for (t in seq(Gapmax.t+GapemaxDur.mean+dt,Tengulf, dt)) {
    #.THETAgape.t <-((THETAgape -(Hsync2*Vc_0)/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*(t-Gapmax.t-GapemaxDur.mean))-THETAgape.t)*100
    #.THETAgape.t <- (0-THETAgape)/(Tengulf - Gapmax.t-GapemaxDur.mean)
    #THETAgape.t<-THETAgape -(Hsync2*Vc_0)/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*(t-Gapmax.t-GapemaxDur.mean) #gape angle at time t if closing EQ7
    THETAgape.t <-(-THETAgape)/(Tengulf-Tclose)*(t-Gapmax.t-GapemaxDur.mean-dt)+THETAgape
    .THETAgape.t<-(-THETAgape)/(Tengulf-Tclose)
    #THETAgape.t<-THETAgape +.THETAgape.t*dt #gape angle at time t if closing EQ7
    
    PHE<-0
    Ac.t<-0.5*pi*Whead/2*(Xjd*Ljaw*sin(deg2rad(THETAgape.t))) # EQ11 mouth area in meters cubed
    
    .Mw_antTMJ.t<-(-Mw_antTMJ/THETAgape)*(.THETAgape.t) #EQ12b
    .Mw.t<-.Mw_antTMJ.t
    Mw.t<-Mw.t+.Mw.t*dt
    Fbc.t<-Kclose*4*Ac.t*Mw.t/(pi*Whead*Topen)# EQ17 Force applied to engulfed mass by buccal cavity walls
    Fed.t<-Fbc.t
    Kam<-0.2
    Fsd.t<-(Cd_close*Ac.t+Cd_body*Abody)*(0.5*Dwater*Vc.t^2)+ 0.2*Dwater*(pi/12)*L0*((Whead/2)+Ljaw)^2*ac.t #EQ21
    #Not for mouth closing Fextthrust<- 4000*(Lbody-0.77)/(14.40-0.77)*(Lbody/14.40)^0.82 #Thrust plus bouyancy
    
    ac.t<-(Fextthrust-Fed.t-Fsd.t)/Mc
    Fww<-Fed.t*(1+(Mw.t/Mc))-Vw.t*.Mw_antTMJ.t #EQ27
    aw.t<- (Fbc.t-Fww-Vw.t*.Mw.t)/Mw.t # 
    ac.t<-aw.t
    Fextthrust.t<-ac.t*Mc+Fsd.t+Fed.t
    Vw.t<-Vw.t+aw.t*dt
    Vc.t<- Vc.t+ac.t*dt
    
    df[n,1]<-t
    df[n,2]<-Ac.t
    df[n,3]<-Mw.t
    df[n,4]<-.Mw.t
    df[n,5]<-ifelse(Vc.t>0,Vc.t,0)
    df[n,6]<-ifelse(Vw.t>0,Vw.t,0)
    df[n,7]<-ac.t
    df[n,8]<-aw.t
    df[n,9]<-THETAgape.t
    df[n,10]<-Fed.t
    df[n,11]<-Fsd.t
    df[n,12]<-Fextthrust.t
    n<-n+1
  }
  df$d.t<-df$Vc.t*dt #distance traveld by teh jaw hinge at time step t
  df$D.t<-NA #total distance traveld by jaw hinge at time step t
  df$Dcapture.area.t<-NA#total distance traveled by the capture area at time t
  for(i in 1:nrow(df)){
    df[i,]$D.t<-sum(df[1:i,]$d.t)
  }
  df$vol.t<-df$d.t*df$Ac.t
  df$Dlj.t<- df$D.t+Xjd*Ljaw*cos(df$THETAgape.t*3.14159/180)# total horizontal distance traveld by the lower jaw at time step t
  df$dlj.t<-NA
  for(i in 2:nrow(df)){
    df[i,]$dlj.t<-df[i,]$Dlj.t-df[i-1,]$Dlj.t
  }
  df$Vlj.t<-df$dlj.t/dt
  df[1,]$Vlj.t<-df[1,]$Vc.t
  df$Vcapture.area.t<-df$Vc.t
  df[df$t> Tclose,]$Vcapture.area.t<-df[df$t> Tclose,]$Vlj.t
  df$dcapture.area.t<-df$Vcapture.area.t*dt
  for(i in 1:nrow(df)){
    df[i,]$Dcapture.area.t<-sum(df[1:i,]$dcapture.area.t)
  }
  assign("df", df, envir = .GlobalEnv)
} # var = humpback-specific values; Prey = options = krill,fish
Capture.prop.stochastic<-function (Vp, Ddetect, Vp.sd, Ddetect.sd, df, lungetype, Tclose, var,grid = 15){
  #Vp = velocity of the prey.  units = m/s. Choose a burst rather than milling speed
  #Ddetect = Distance at which prey react to a predator (begin to swim away). units = m
  # Vp.sd = unbiased population standard deviation of Vp
  #Ddetect.sd = unbiased population standard deviation of Ddetect
  #df = output of the Create df. function.  A dataframe containing many hydrodynamic characteristics at various points in the lunges
  #lungetype = Catagorical factor variable.  Options are "subsurface, lateral, or group surface." THis allows for modeling different escape paths of prey 
  #Tclose = The time at which the whale's mouth begins to close.  Also an output of the Create.df
  #var = a dataframe with allometrically-scaled morphological parameters for engulfment
  
  if (lungetype == "subsurface"){
    df$capture.prob<-NA
    df$escape.dist<-NA
    for (r in 2:nrow(df)){
      print(r)
      r1<-Whead/2
      r2<-Xjd*Ljaw*sin(deg2rad(df[r,]$THETAgape.t))
      x<- seq (from = -r1, to = r1, length.out = grid)
      y<- seq (0, r2, length.out = grid)
      points<-expand.grid(x,y)
      #plot(points$Var1,points$Var2)
      points<-points[points$Var1^2/(r1^2)+ points$Var2^2/r2^2 <=1, ]
      points$dist<-NA
      for (n in 1:nrow(points)){
        if(points[n,]$Var1^2/r1^2+points[n,]$Var2^2/r2^2 ==1){
          points[n,]$dist<-0}
        else {
          dist1<-sqrt((points[n,]$Var1-0)^2+(points[n,]$Var2-sqrt((r1^2*r2^2-0^2*r2^2)/r1^2))^2)
          x<-0+0.01
          y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
          dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
          if (dist1>dist2 & abs(x)< Whead){
            while (dist1>dist2 & abs(x)< r1){
              dist1<-dist2
              x<-x+0.01
              y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
              dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
              #print(dist2)
            }
          }else {
            x<-0-0.01
            y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
            dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
            while (dist1>=dist2 & abs(x)< r1){
              dist1<-dist2
              y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
              dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
              x<-x-0.01
            }
          }
          points[n,]$dist<-dist1
          #print(n)
        }
      }
      #ggplot()+geom_contour(aes(x = Var1, y = Var2, z = dist),data = points)+theme_classic()
      #hist(points$dist)
      df[r,]$escape.dist<- Vp*Ddetect/mean(df[df$Dcapture.area.t< df[r,]$Dcapture.area.t & df$Dcapture.area.t > df[r,]$Dcapture.area.t - Ddetect,]$Vcapture.area.t)
      points$prob<-pnorm(points$dist, df[r,]$escape.dist, sqrt((Ddetect.sd^2*Vp.sd^2+Vp.sd^2*Ddetect^2+Ddetect^2*Vp^2)*(1/mean(df[df$Dcapture.area.t< df[r,]$Dcapture.area.t & df$Dcapture.area.t > df[r,]$Dcapture.area.t - Ddetect,]$Vcapture.area.t))^2))
      #plot 
      #p1<-ggplot(aes(x = Var1, y = -Var2),data = points)+geom_contour(aes(z = prob,colour = ..level..),size = 1,data = points)+scale_colour_gradient(limits=c(0, 1))+
       # theme_classic()+theme(aspect.ratio=max(points$Var2)/max(points$Var1*2),
                            #  panel.border = element_blank(),
                             # panel.grid.major = element_blank(),
                              #panel.grid.minor = element_blank(),
                              #axis.line.x = element_line(color="black", size = 0.5),
                              #axis.line.y = element_line(color="black", size = 0.5),
                              #panel.background = element_blank(),text = element_text(size=12))
       #direct.label(p1, list("bottom.pieces", colour='black'))
       #ggsave("StochasticMod2.tiff", path = "C:/Users/Ellen/Desktop/Analysis/Chapter3/", width =3.5, height = 4.5, unit = "in")
       
      #p2<-ggplot(aes(x = Var1, y = -Var2),data = points)+geom_point(aes(color = prob),size = 2)+
     #   theme_classic()+theme(aspect.ratio=max(points$Var2)/max(points$Var1*2),
     # panel.border = element_blank(),
     # panel.grid.major = element_blank(),
     # panel.grid.minor = element_blank(),
     # axis.line.x = element_line(color="black", size = 0.5),
     # axis.line.y = element_line(color="black", size = 0.5),
     # panel.background = element_blank(),text = element_text(size=12))+scale_colour_gradient(limits=c(0, 1))
      #ggsave("StochasticMod.tiff",p2, path = "C:/Users/Ellen/Desktop/Analysis/Chapter3/", width =3.5, height = 4.5, unit = "in")
      df[r,]$capture.prob<-mean(points$prob,na.rm = TRUE)##}
    }
    
    df2<-df[df$Dcapture.area.t< max(df$Dcapture.area.t)-df[nrow(df),]$escape.dist,]
    assign("prop_captured",sum(df2$Ac.t*df2$Vcapture.area.t*dt*df2$capture.prob, na.rm = TRUE)/sum(df$Ac.t*df$Vcapture.area.t*dt, na.rm = TRUE), envir = .GlobalEnv)
    }
  if (lungetype == "lateral"){
    df$capture.prob<-NA
    df$escape.dist<-NA
    for (r in 2:nrow(df)){
      print(r)
      r1<-Whead/2
      r2<-Xjd*Ljaw*sin(deg2rad(df[r,]$THETAgape.t))
      x<- seq (from = -r1, to = r1, length.out = grid)
      y<- seq (0, r2, length.out = grid)
      points<-expand.grid(x,y)
      #plot(points$Var1,points$Var2)
      points<-points[points$Var1^2/(r1^2)+ points$Var2^2/r2^2 <=1, ]
      points$dist<-NA
      for (n in 1:nrow(points)){
        if(points[n,]$Var1^2/r1^2+points[n,]$Var2^2/r2^2 ==1 & points[n,]$Var1>0){
          points[n,]$dist<-0}
        else {
          dist1<-sqrt((points[n,]$Var1-0)^2+(points[n,]$Var2-sqrt((r1^2*r2^2-0^2*r2^2)/r1^2))^2)
          x<-0+0.01
          y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
          dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
          while (dist1>dist2 & abs(x)< r1){
            dist1<-dist2
            x<-x+0.01
            y<-sqrt((r1^2*r2^2-x^2*r2^2)/r1^2)
            dist2<-sqrt((points[n,]$Var1-x)^2+(points[n,]$Var2-y)^2)
            #print(dist2)
          }
        }
        points[n,]$dist<-dist1
        #print(n)
      
      }
      #ggplot()+geom_contour(aes(x = Var1, y = Var2, z = dist),data = points)
      #hist(points$dist)
      df[r,]$escape.dist<- Vp*Ddetect/mean(df[df$Dcapture.area.t< df[r,]$Dcapture.area.t & df$Dcapture.area.t > df[r,]$Dcapture.area.t - Ddetect,]$Vcapture.area.t)
      points$prob<-pnorm(points$dist, df[r,]$escape.dist, sqrt((Ddetect.sd^2*Vp.sd^2+Vp.sd^2*Ddetect^2+Ddetect^2*Vp^2)*(1/mean(df[df$Dcapture.area.t< df[r,]$Dcapture.area.t & df$Dcapture.area.t > df[r,]$Dcapture.area.t - Ddetect,]$Vcapture.area.t))^2))
      #ggplot()+geom_contour(aes(x = Var1, y = Var2, z = dist),size = 1,data = points)+geom_point(aes(x = Var1, y = Var2),data = points)      
      df[r,]$capture.prob<-mean(points$prob,na.rm = TRUE)
        p1<-ggplot(aes(x = -Var2, y = -Var1),data = points)+geom_contour(aes(z = prob,colour = ..level..),size = 1,data = points)+scale_colour_gradient(limits=c(0, 1))+
      theme_classic()+theme(aspect.ratio=max(points$Var1*2)/max(points$Var2),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color="black", size = 0.5),
      axis.line.y = element_line(color="black", size = 0.5),
      panel.background = element_blank(),text = element_text(size=12))
direct.label(p1, list("bottom.pieces", colour='black'))
ggsave("StochasticMod3.tiff", path = "C:/Users/Ellen/Desktop/Analysis/Chapter3/", width =3.5, height = 4.5, unit = "in")

    ggplot(aes(x =-Var2, y =  -Var1),data = points)+geom_point(aes(color = prob),size = 2)+theme_classic()+scale_colour_gradient(limits=c(0, 1))+theme(aspect.ratio=max(points$Var1*2)/max(points$Var2),
                                                                                                                 panel.border = element_blank(),
                                                                                                                 panel.grid.major = element_blank(),
                                                                                                                 panel.grid.minor = element_blank(),
                                                                                                                 axis.line.x = element_line(color="black", size = 0.5),
                                                                                                                 axis.line.y = element_line(color="black", size = 0.5),
                                                                                                                 panel.background = element_blank(),text = element_text(size=12))                                                                                                          
ggsave("StochasticMod4.tiff", path = "C:/Users/Ellen/Desktop/Analysis/Chapter3/", width =3.5, height = 4.5, unit = "in")
      }
    
    #df2<-df[df$Dcapture.area.t< max(df$Dcapture.area.t)-df[nrow(df),]$escape.dist,]
    df2<-df
    assign("prop_captured",sum(df2$Ac.t*df2$Vcapture.area.t*dt*df2$capture.prob, na.rm = TRUE)/sum(df$Ac.t*df$Vcapture.area.t*dt, na.rm = TRUE), envir = .GlobalEnv)
  }
  if(lungetype == "group lunge"){
    df$capture.prob<-1
    assign("prop_captured",1,envir = .GlobalEnv)
  }
  assign("df",df,envir = .GlobalEnv)
}
Energy_gain.nodf<-function(Vp = NA, Vp.sd = NA, Ddetect = NA, Ddetect.sd= NA, assim_eff = 0.84,
                           mass_ind,energy_g, Sv_mean,TS,lungetype = NA,var = var,Prey = NA, 
                           Data = 1,Vc_0 = NA, proportion_captured = "formula", grid = NA){
  if(proportion_captured == "formula"){
    df<-Create.df(var = var, Prey = Prey, Vc_0 = Vc_0)
    Capture.prop.stochastic(Vp=Vp, Ddetect=Ddetect, Vp.sd=Vp.sd, Ddetect.sd=Ddetect.sd, df=df, lungetype=lungetype, var=var, grid = grid)
  }
  if(proportion_captured != "formula"){prop_captured<-proportion_captured}
  engulf_vol_m3<-(8.0094*Lbody ^3.21)/1020 # equation from GOldbogen et al 2012 for kg.  / 1020 to accoutn for weight of seawater
  
  Data<-as.data.frame(Data)
  Data$.ind_m3<-10^(Sv_mean/10)/10^(TS/10)
  Data$.ind_captured<-10^(Sv_mean/10)/10^(TS/10)*prop_captured*engulf_vol_m3
  Data$.biomass_m3<-10^(Sv_mean/10)/10^(TS/10)*mass_ind
  Data$.biomass_captured<- Data$.ind_captured*mass_ind
  Data$.energy_gain<- Data$.biomass_captured*energy_g*assim_eff
  Data$.prop_captured<-prop_captured
  Data$.Vp<-Vp
  Data$.mass_ind<-mass_ind
  Data$.energy_g<-energy_g
  Data$.lungetype<-lungetype
  Data$.prey<-Prey
  print(Data)
}

##Read inputs##
setwd("~/Analysis/Behavior Obs")
var<-read.csv("Chapter2Model2.csv")
EnergyParameters.keep<-EnergyParameters
EnergyParameters<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/Chapter3ModParValues.csv")
EnergyParameters$Prey<-as.character(EnergyParameters$Prey)
#EnergyParameters$CaptureProp_subsurface<-NA
#EnergyParameters$CaptureProp_surface<-NA
#EnergyParameters<-EnergyParameters[1:10,]


max<-nrow(EnergyParameters)
for (a in 1:10){
  print(a)
  if(!is.na(EnergyParameters[a,]$Whale.initial.speed..subsurface.)){
  Create.df(var = var,Prey = EnergyParameters[a,]$Prey, Vc_0 = EnergyParameters[a,]$Whale.initial.speed..subsurface.)
  Capture.prop.stochastic(Vp = EnergyParameters[a,]$Prey.speed,
                          Vp.sd = EnergyParameters[a,]$SD.Prey.speed,
                          Ddetect = EnergyParameters[a,]$Mean.Reaction.distance,
                          Ddetect.sd = EnergyParameters[a,]$SD.Reaction.distance,
                          df = df,
                          lungetype="subsurface",
                          Tclose=Tclose,
                          var = var)
  EnergyParameters[a,]$CaptureProp_subsurface<-prop_captured
  }
  if(!is.na(EnergyParameters[a,]$Whale.initial.speed..surface.)){
  Create.df(var = var,Prey = EnergyParameters[a,]$Prey, Vc_0 = EnergyParameters[a,]$Whale.initial.speed..surface.)
  Capture.prop.stochastic(Vp = EnergyParameters[a,]$Prey.speed,
                             Vp.sd = EnergyParameters[a,]$SD.Prey.speed,
                             Ddetect = EnergyParameters[a,]$Mean.Reaction.distance,
                             Ddetect.sd = EnergyParameters[a,]$SD.Reaction.distance,
                             df = df,
                             lungetype="lateral",
                             Tclose=Tclose,
                             var = var)
  EnergyParameters[a,]$CaptureProp_surface<-prop_captured
  }
}

#Acoustics
Herring<-read.csv("H:/Analysis/Chapter3Lunges_Tenakee_Seymour4.csv")
SS2012Krill<-read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")
SS2012Krill<-SS2012[SS2012$Sv_mean_linear>0.0000000001,]
Herring<-read.csv("H:/Analysis/Chapter3Lunges_Tenakee_Seymour4.csv")
TI2013Herring_large<-Herring[(Herring$tag=="mn13_112a"|Herring$tag=="mn13_113a"|Herring$tag=="mn13_111a"|Herring$tag=="mn13_113d" & Herring$dive<=38),]
SC2014Herring<-Herring[startsWith(as.character(Herring$tag),"mn14") & !Herring$tag == "mn14_114a",]
TI2013Herring_small<-Herring[startsWith(as.character(Herring$tag),"mn13")&!(Herring$tag=="mn13_112a"|Herring$tag=="mn13_113a"|Herring$tag=="mn13_111a"|Herring$tag=="mn13_113d" & Herring$dive<=38),]
KB2014Herring<-read.csv("H:/Analysis/Chapter3/Acoustic Data/Herring Regions June2_Sv.csv")
KB2014Herring<-KB2014Herring[1:26,] 
CS2014Chum<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter 2/New folder/Schools_2.csv")
CS2014Chum<-CS2014Chum[c(1:8),]
KB2014Coho<-10*log10((1.8*10^-5)/2)
SC2014Krill<-Herring[Herring$tag == "mn14_114a",]
SC2014Krill$Sv_mean<--62.6
CS2014Coho<- -50
EnergyParameters$Sv<-as.character(EnergyParameters$Sv)
EnergyParameters$Data<-as.character(EnergyParameters$Data)
CS2014Coho<-as.data.frame(CS2014Coho)
CS2014Coho$Sv_mean<--50

none<-1
Energygain<-c()
for(n in 1:nrow(EnergyParameters)){
  n
  assign(paste(EnergyParameters[n,]$Prey.Group),Energy_gain.nodf(mass_ind = EnergyParameters[n,]$Mean.Wet.mass,
                   energy_g = EnergyParameters[n,]$Energy,
                   Sv_mean = get(EnergyParameters[n,]$Sv)$Sv_mean,
                   proportion_captured = EnergyParameters[n,]$CaptureProportion_WA,
                   TS = EnergyParameters[n,]$TS,
                   var = var,
                   lungetype = "subsurface",
                   Prey = EnergyParameters[n,]$Prey,
                   Data = get(EnergyParameters[n,]$Data)) )
}
 
  
HerringTI_large$group<-"HerringTI_large"
HerringTI_small$group<-"HerringTI_small"

HerringSC$group<- "HerringSC"
KrillSS$group<-"KrillSS"
KrillSC$group<-"KrillSC" 
ChumEarly$group<-"ChumKB"
ChumLate$group<- "ChumTB"
HerringKB$group<-"HerringKB"
CohoAtRelease$group<-"CohoAtRelease"
CohoAfterRelease$group<-"CohoAfterRelease"

EnergyAll<-rbind.fill(HerringTI_large, HerringTI_small,
                      HerringSC, 
                      KrillSS, 
                      KrillSC, 
                      ChumEarly,
                      ChumLate, 
                      HerringKB, 
                      CohoAtRelease, 
                      CohoAfterRelease)

#EnergyAll<-EnergyAll[EnergyAll$quality<2,]
EnergyAll<-EnergyAll[log(EnergyAll$.energy_gain)>0,]
EnergyAll$group<-as.factor(EnergyAll$group)
ggplot()+geom_boxplot(aes(group,.energy_gain), data = EnergyAll)+scale_y_log10("Energy Captured (kJ)",labels = scales::comma)
ggplot()+geom_point(data = EnergyAll,aes(group,.prop_captured, color = group, size = 3))
ggplot()+geom_point(data = EnergyAll,aes(.biomass_m3,.energy_gain, color = group))+geom_smooth(aes(.biomass_m3,.energy_gain),method = "lm", data = EnergyAll)
ggplot()+geom_point(data = EnergyAll,aes(.energy_g,.energy_gain, color = group))+geom_smooth(aes(.energy_g,.energy_gain),method = "lm", data = EnergyAll)
ggplot()+geom_point(data = EnergyAll,aes(.energy_g,log(.energy_gain), color = group))+geom_smooth(aes(.energy_g,log(.energy_gain)),method = "lm", data = EnergyAll)

EnergyAll$.energy_m3<-EnergyAll$.biomass_m3*EnergyAll$.energy_g
results<-as.data.frame(summaryBy(.energy_m3+.prop_captured+.energy_gain ~group, FUN = c(mean,se), data = EnergyAll, na.rm = TRUE))
results<-results[order(-results$.energy_gain.mean),]
results$gross.energy_gainkJ.Lunge.mean<-results$.energy_gain.mean
results
results.keep<-results
