#See Potvin et al 2012 in PLOSone Figure 3
#libraryHeight
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("plyr", lib.loc="~/R/win-library/3.4")
##functions##
assignVec <- Vectorize("assign",c("x","value"))
deg2rad <- function(deg) {(deg * pi) / (180)}
se <- function(x, na.rm = TRUE) {
  
  sd(x,na.rm = )/sqrt(sum(!is.na(x)))}
# modeling prey capture:
Prop_captured <- function(Vp, Ddetect, df, lungetype,Tclose) {
  if(lungetype == "subsurface"){
  #df$prey_void<- (3.14159*(Vp*Ddetect/df$Vc.t)^2)/2
  #df[df$prey_void>df$Ac.t,]$prey_void<-df[df$prey_void>df$Ac.t,]$Ac.t
  x<-Vp*(Tclose)
  df[df$t>Tclose,]$prey_void<-0
  df[df$Dlj.t> max(df$Dlj.t)-x,]$prey_void<-df[df$Dlj.t> max(df$Dlj.t)-x,]$Ac.t
  }
  
  if(lungetype == "lateral"){
    #df$prey_void<- (3.14159*(Vp*Ddetect/df$Vc.t)^2)/4
    #df[df$prey_void>df$Ac.t,]$prey_void<-df[df$prey_void>df$Ac.t,]$Ac.t
  x<-Vp*(Tclose)
  df[df$t>Tclose,]$prey_void<-0
  df[df$Dlj.t> max(df$Dlj.t)-x]$prey_void<-df[df$prey_void>df$Ac.t,]$Ac.t
}

  if(lungetype == "vertical"){
  #df$prey_void<- (3.14159*(Vp*Ddetect/df$Vc.t)^2)/2
  #df[df$prey_void>df$Ac.t,]$prey_void<-df[df$prey_void>df$Ac.t,]$Ac.t
x<-Vp*(Tclose)
#df[df$t>Tclose,]$prey_void<-0
df[df$Dlj.t> max(df$Dlj.t)-x]$prey_void<-0 #redundant to the above
}
  assign("df",df,envir = .GlobalEnv)
  assign("prop_captured",sum(df$Ac.t-df$Ap.t)/sum(df$Ac.t), envir = .GlobalEnv)
}

Capture_volume<-function(Vp, Ddetect, df, lungetype, Tclose, var) {
  df$Ap.t<-NA
  if(lungetype == "subsurface"){
  for (i in 2:(nrow(df[df$t<Tclose,]))){
    #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
    df[i,]$Ap.t<-0.5*pi*
    (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))*
                                ((Xjd*Ljaw*sin(deg2rad(df[i,]$THETAgape.t)))-
              (Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))#recaculating the area of the elipse of captured prey 
  }
  for (j in i:(nrow(df))){
    #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
    df[j,]$Ap.t<-0.5*pi*
      (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))*
      ((Xjd*Ljaw*sin(deg2rad(df[j,]$THETAgape.t)))-
         (Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))#recaculating the area of the elipse of captured prey 
  }
    x<- Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)
    df[df$Dlj.t> max(df$Dlj.t)-x,]$Ap.t<-0
    
  }
  
  if(lungetype == "lateral"){
    for (i in 2:(nrow(df[df$t<Tclose,]))){
      #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
      df[i,]$Ap.t<-(0.25*pi)* #prey on underwater side
        (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))*
        ((Xjd*Ljaw*sin(deg2rad(df[i,]$THETAgape.t)))-
           (Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))+
        
        #prey on underwater side
        (0.25*pi)*(Whead/2)*(Xjd*Ljaw*sin(deg2rad(df[i,]$THETAgape.t)))
          
        #recaculating the area of the elipse of captured prey 
    
      }
    for (j in i:(nrow(df))){
      #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
      df[j,]$Ap.t<-(0.25*pi)*
        (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))*
        ((Xjd*Ljaw*sin(deg2rad(df[j,]$THETAgape.t)))-
           (Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))+
        (0.25*pi)*(Whead/2)*(Xjd*Ljaw*sin(deg2rad(df[j,]$THETAgape.t)))
        
        #recaculating the area of the elipse of captured prey 
    }
    x<- Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t) 
    df[df$Dlj.t> max(df$Dlj.t)-x,]$Ap.t<-0
    
  }
  
  if(lungetype == "vertical"){
    for (i in 2:(nrow(df[df$t<Tclose,]))){
      #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
      df[i,]$Ap.t<-0.5*pi*
        (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))*
        ((Xjd*Ljaw*sin(deg2rad(df[i,]$THETAgape.t)))-
           (Vp*Ddetect/mean(df[df$D.t<df[i,]$D.t & df$D.t>df[i,]$D.t - Ddetect,]$Vc.t)))#recaculating the area of the elipse of captured prey 
    }
    for (j in i:(nrow(df))){
      #calculating the half elipse of captured prey by decreasing the radii by the escape distance (preyspeed *Ddist/mean whale speed over detection distance)
      df[j,]$Ap.t<-0.5*pi*
        (Whead/2-(Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))*
        ((Xjd*Ljaw*sin(deg2rad(df[j,]$THETAgape.t)))-
           (Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)))#recaculating the area of the elipse of captured prey 
    }
    x<- Vp*Ddetect/mean(df[df$D.t<df[j,]$D.t & df$D.t>df[j,]$D.t - Ddetect,]$Vlj.t)
    df[df$Dlj.t> max(df$Dlj.t)-x,]$Ap.t<-df[df$Dlj.t> max(df$Dlj.t)-x,]$Ac.t
    
  }
  
  if(lungetype == "group lunge"){
    df$Ap.t<-df$Ac.t
    
  }
  
  if(length(df[df$Ap.t<0|is.na(df$Ap.t),]$Ap.t)>0) {
    df[df$Ap.t<0|is.na(df$Ap.t),]$Ap.t<-0}
  df[df$Ap.t>df$Ac.t,]$Ap.t<-df[df$Ap.t>df$Ac.t,]$Apc.t
  assign("df",df,envir = .GlobalEnv)
  assign("prop_captured",sum(df$Ap.t)/sum(df$Ac.t), envir = .GlobalEnv)
}

Create.df<- function(var,Prey, Vc_0){
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
    assign("Vc.t",Vc_0,,envir = .GlobalEnv)
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
Tengulf<- (Xjd*Ljaw*sin(deg2rad(THETAgape))*THETAgape/(Hsync*Vc_0))*2  #EQ 8 
Mw_postTMJ<-Dwater*pi/3*(L0-Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 9 engulfed mass post TMJ
Mw_antTMJ<-Dwater*pi/3*(Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 10
Mw<-Mw_postTMJ+Mw_antTMJ

  
if (Prey == "krill") {
Tengulf.mean<-2.0
  Tengulf.sd<-0.5
  GapemaxDur.mean<-0.5
  GapemaxDur.sd<-0.6
  Tengulf<-2
  Gapmax.t<-0.7
  Topen<-Gapmax.t
  #Vc_0<-2.7 #4 
}

if (Prey == "fish") {
  Tengulf.mean<-4.8
  Tengulf.sd<-3.0
  GapemaxDur.mean<-2.1
  GapemaxDur.sd<-2.9
  Tengulf<-4.8
  Gapmax.t<-1.2
  Topen<-Gapmax.t
  #Vc_0<-2.5
}
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

assign("df", df, envir = .GlobalEnv)
} # var = humpback-specific values; Prey = options = krill,fish

Energy_gain.nodf<-function(Vp, Ddetect, assim_eff, engulf_vol_m3,mass_ind,energy_g, Sv_mean,TS,lungetype,var,Prey, Data,Vc_0, proportion_captured){
  if(proportion_captured == "formula"){
  df<-Create.df(var = var, Prey = Prey, Vc_0 = Vc_0)
  Capture_volume(Vp = Vp, Ddetect = Ddetect, df = df, lungetype = lungetype, Tclose = Tclose, var = var)
  }
  if(proportion_captured != "formula"){prop_captured<-proportion_captured}
  
  
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
EnergyParameters<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter3/EnergyParameters.csv")

SS2012<-read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")
SS2012<-SS2012[SS2012$Sv_mean_linear>0.0000000001,]

#proportion_captured<-1
proportion_captured<-"formula"

KrillSS<-Energy_gain.nodf(Vp = 0.14, 
              Ddetect = 0.9, 
              lungetype = "subsurface", 
              assim_eff = 0.92, 
              engulf_vol_m3 = 42,
              mass_ind = EnergyParameters[4,]$Wet_mass_g,
              energy_g = EnergyParameters[4,]$Energy_kJ.g,
              Sv_mean = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")$Sv_mean,
              TS = -85,#Cade estimate.  Needs to be addressed
              var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
              Prey = "krill",
              Data = read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv"),
              Vc_0 = 4.6,#cade for krill
              proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)

Herring<-read.csv("H:/Analysis/Chapter3Lunges_Tenakee_Seymour3.csv")

HerringTI<-Energy_gain.nodf(Vp = 1.24, #Boswell
                 Ddetect = 4, #Boswell
                 lungetype = "group lunge", 
                 assim_eff = 0.92, 
                 engulf_vol_m3 = 42,
                 mass_ind = EnergyParameters[1,]$Wet_mass_g,
                 energy_g = EnergyParameters[1,]$Energy_kJ.g,
                 Sv_mean = Herring[startsWith(as.character(Herring$tag),"mn13"),]$Sv_mean,
                 TS = 20*log10(EnergyParameters[1,]$Length_mm/10)-2.3*log10(1+Herring[startsWith(as.character(Herring$tag),"mn13"),]$Prey.Depth_mean/10)-65.4 ,#Ona 2003
                 var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                 Prey = "fish",
                 Data = Herring[startsWith(as.character(Herring$tag),"mn13"),],
                 Vc_0 = 3.6,
                 proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)

HerringSC<-Energy_gain.nodf(Vp = 1.24, #He et al 1993 17.1 L/s at mean 0.217 m (sd 0.021) temp?
                            Ddetect = 4, 
                            lungetype = "subsurface", 
                            assim_eff = 0.92, 
                            engulf_vol_m3 = 42,
                            mass_ind = EnergyParameters[2,]$Wet_mass_g,
                            energy_g = EnergyParameters[2,]$Energy_kJ.g,
                            Sv_mean = Herring[startsWith(as.character(Herring$tag),"mn14") & Herring$tag != "mn14_114a",]$Sv_mean,
                            TS = 20*log10(EnergyParameters[2,]$Length_mm/10)-2.3*log10(1+Herring[startsWith(as.character(Herring$tag),"mn14") & Herring$tag != "mn14_114a",]$Prey.Depth_mean/10)-65.4 ,#Ona 2003
                            var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                            Prey = "fish",
                            Data = Herring[startsWith(as.character(Herring$tag),"mn14") & Herring$tag != "mn14_114a",],
                            Vc_0 = 3.6,
                            proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)

KrillSC<-Energy_gain.nodf(Vp = 0.14, #He et al 1993 17.1 L/s at mean 0.217 m (sd 0.021) temp?
                            Ddetect = 0.9, 
                            lungetype = "subsurface", 
                            assim_eff = 0.92, 
                            engulf_vol_m3 = 42,
                            mass_ind = EnergyParameters[5,]$Wet_mass_g,
                            energy_g = EnergyParameters[5,]$Energy_kJ.g,
                            Sv_mean = Herring[Herring$tag == "mn14_114a",]$Sv_mean,
                            TS = -85 ,#estimate
                            var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                            Prey = "krill",
                            Data = Herring[Herring$tag == "mn14_114a",]$max.speed,
                          Vc_0 = 4.2 ,
                          proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)#cade for krill


CS2014<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter 2/New folder/Schools_2.csv")
CS2014<-CS2014[c(1:8),]

ChumKB<-Energy_gain.nodf(Vp = 0.8, #He et al 1993 17.1 L/s at mean 0.217 m (sd 0.021) temp?
                            Ddetect = 0.9, 
                            lungetype = "lateral", 
                            assim_eff = 0.92, 
                            engulf_vol_m3 = 42,
                            mass_ind = EnergyParameters[6,]$Wet_mass_g,
                            energy_g = EnergyParameters[6,]$Energy_kJ.g,
                            Sv_mean = CS2014$Sv_mean,
                            TS = -52 ,# 
                            var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                            Prey = "fish",
                            Data = CS2014,
                            Vc_0 = 2.5,
                         proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)#cade for fish 

ChumTB<-Energy_gain.nodf(Vp = 0.92, #He et al 1993 17.1 L/s at mean 0.217 m (sd 0.021) temp?
                         Ddetect = 0.9, 
                         lungetype = "lateral", 
                         assim_eff = 0.92, 
                         engulf_vol_m3 = 42,
                         mass_ind = EnergyParameters[7,]$Wet_mass_g,
                         energy_g = EnergyParameters[7,]$Energy_kJ.g,
                         Sv_mean = CS2014$Sv_mean,
                         TS = -51 ,#estimate
                         var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                         Prey = "fish",
                         Data = CS2014,
                         Vc_0 = 2.5,
                         proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs)#cade for fish

Herring2014KB<-read.csv("H:/Analysis/Chapter3/Acoustic Data/Herring Regions June2_Sv.csv")
Herring2014KB<-Herring2014KB[1:26,]
HerringKB<-Energy_gain.nodf(Vp = 1.24, #He et al 1993 17.1 L/s at mean 0.217 m (sd 0.021) temp?
                         Ddetect = 4, 
                         lungetype = "subsurface", 
                         assim_eff = 0.92, 
                         engulf_vol_m3 = 42,
                         mass_ind = EnergyParameters[3,]$Wet_mass_g,
                         energy_g = EnergyParameters[3,]$Energy_kJ.g,
                         Sv_mean = Herring2014KB$Sv_mean,
                         TS = 20*log10(EnergyParameters[3,]$Length_mm/10)-2.3*log10(1+Herring2014KB$Depth_mean/10)-65.4,
                         var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                         Prey = "fish",
                         Data = Herring2014KB,
                         Vc_0 = 3.6,
                         proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs) #mean(Herring[Herring$tag != "mn14_114a",]$max.speed, na.rm = TRUE))

CohoAtRelease<-Energy_gain.nodf(Vp = 1.2, 
                         Ddetect = 0.9, 
                         lungetype = "lateral", 
                         assim_eff = 0.92, 
                         engulf_vol_m3 = 42,
                         mass_ind = EnergyParameters[8,]$Wet_mass_g,
                         energy_g = EnergyParameters[8,]$Energy_kJ.g,
                         Sv_mean = 10*log10((1.8*10^-5)/2),#converting modeles max sa (1.8*10^-5) to Sv_mean 
                         TS = -45 ,#estimate
                         var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                         Prey = "fish",
                         Data = 1,
                         Vc_0 = 2.5,
                         proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs) #Cade for FIsh)
CohoAfterRelease<-Energy_gain.nodf(Vp = 0.92, 
                                Ddetect = 4, 
                                lungetype = "lateral", 
                                assim_eff = 0.92, 
                                engulf_vol_m3 = 42,
                                mass_ind = EnergyParameters[9,]$Wet_mass_g,
                                energy_g = EnergyParameters[9,]$Energy_kJ.g,
                                Sv_mean = 10*log10((1.8*10^-5)/2),#converting modeles max sa (1.8*10^-5) to Sv_mean 
                                TS = -46 ,#estimate
                                var = read.csv("~/Analysis/Behavior Obs/Chapter2Model2.csv"),
                                Prey = "fish",
                                Data = 1,
                                Vc_0 = 2.5,#Cade for FIsh
                                proportion_captured = proportion_captured) # insert value or specify "formula" to calcualte based on inputs) 


EnergyAll<-EnergyAll[EnergyAll$quality<2,]
HerringTI$group<-"HerringTI"
HerringSC$group<- "HerringSC"
KrillSS$group<-"KrillSS"
KrillSC$group<-"KrillSC" 
ChumKB$group<-"ChumKB"
ChumTB$group<- "ChumTB"
HerringKB$group<-"HerringKB"
CohoAtRelease$group<-"CohoAtRelease"
CohoAfterRelease$group<-"CohoAfterRelease"

EnergyAll<-rbind.fill(HerringTI, 
                      HerringSC, 
                      KrillSS, 
                      KrillSC, 
                      ChumKB,
                      ChumTB, 
                      HerringKB, 
                      CohoAtRelease, 
                      CohoAfterRelease)

EnergyAll<-EnergyAll[log(EnergyAll$.energy_gain)>0,]
EnergyAll$group<-as.factor(EnergyAll$group)
ggplot()+geom_boxplot(aes(group,.energy_gain), data = EnergyAll)+scale_y_log10("Energy Captured (kJ)",labels = scales::comma)
ggplot()+geom_point(data = EnergyAll,aes(group,.prop_captured, color = group, size = 3))
ggplot()+geom_point(data = EnergyAll,aes(.biomass_m3,.energy_gain, color = group))+geom_smooth(aes(.biomass_m3,.energy_gain),method = "lm", data = EnergyAll)
ggplot()+geom_point(data = EnergyAll,aes(.energy_g,.energy_gain, color = group))+geom_smooth(aes(.energy_g,.energy_gain),method = "lm", data = EnergyAll)
ggplot()+geom_point(data = EnergyAll,aes(.energy_g,log(.energy_gain), color = group))+geom_smooth(aes(.energy_g,log(.energy_gain)),method = "lm", data = EnergyAll)

EnergyAll$.energy_m3<-EnergyAll$.biomass_m3*EnergyAll$.energy_g
results<-as.data.frame(summaryBy(.energy_m3+.prop_captured+.energy_gain ~group, FUN = c(mean,se), data = EnergyAll))
results<-results[order(-results$.energy_gain.mean),]
results$gross.energy_gainkJ.Lunge.mean<-results$.energy_gain.mean



##################
#Speeds during lunge by group
AllLunges<-read.csv("H:/Analysis/Chapter3/Tags/AllLunges.csv")
ggplot()+geom_boxplot(aes(group,delta.speed), data = AllLunges)
AllLunges$Vmax2.Vend2<-as.numeric(as.character(AllLunges$Vmax2.Vend2))
AllLunges.sum <- as.data.frame(summaryBy(delta.p+delta.speed+max.speed+Vmax2.Vend2~group,FUN = c(median, mean),data = AllLunges[!is.na(AllLunges$max.speed),]))
AllLunges.sum

#add respiration rate and lunge rate info
#AllTags6<-read.csv("H:/Analysis/Chapter3/Tags/AllTags_7.csv")
AllTags6<-read.csv("C:/Users/Ellen/Desktop/AllTags_7.csv")

AllTags6$prey<-as.factor(AllTags6$prey)
AllTags6$respr.rate_min<-as.numeric(as.character(AllTags6$respr.rate_min))
AllTags6$feeding.<-as.character(AllTags6$feeding.)
AllTags6[AllTags6$lunges>0 & !is.na(AllTags6$lunges),]$feeding.<-"l"
AllTags6$feeding.<-as.factor(AllTags6$feeding.)
AllTags6$feeding.2<-as.factor(AllTags6$feeding.)
AllTags6[AllTags6$feeding.2 == "y",]$feeding.2<-"l"
ggplot(AllTags6,aes(dive.start.sec,lunges , color= feeding., shape = prey))+geom_point()+ facet_wrap(~tag, ncol = 1)

#Breaths/lunge
Rates<-summaryBy(lunges+breaths+total_dur~tag+feeding.+group, FUN = c(sum), data = AllTags6[ !is.na(AllTags6$lunges) & !is.na(AllTags6$breaths)& !is.na(AllTags6$total_dur),] )
Rates<-as.data.frame(Rates)
Rates$lunges_min<-Rates$lunges.sum/(Rates$total_dur.sum/60)
Rates$breaths_min<-Rates$breaths.sum/(Rates$total_dur.sum/60)
ggplot()+geom_point(aes(tag, breaths_min, color = feeding.), size = 2, data = Rates)
Rates2<-summaryBy(lunges_min+breaths_min~group*feeding.,FUN = mean,data = Rates)
Rates2<-as.data.frame(Rates2)
Rates2<-Rates2[Rates2$feeding. == "l",]
Rates2$breaths_lunge<-Rates2$breaths_min.mean/Rates2$lunges_min.mean
Breaths_Lunge<-Rates2
Breaths_Lunge<-Breaths_Lunge[,c(1,5)]

#lunges/min
Rates<-summaryBy(lunges+breaths+total_dur~tag+feeding.2+group, FUN = c(sum), data = AllTags6[ !is.na(AllTags6$lunges) & !is.na(AllTags6$breaths)& !is.na(AllTags6$total_dur),] )
Rates<-as.data.frame(Rates)
Rates$lunges_min<-Rates$lunges.sum/(Rates$total_dur.sum/60)
Rates2<-summaryBy(lunges_min~group,FUN = mean,data = Rates[Rates$feeding.2 == "l",])
Rates2<-as.data.frame(Rates2)
Lunges_min<-Rates2

dat<-merge(Breaths_Lunge, Lunges_min, by = "group")
AddData<-merge(dat,as.data.frame(AllLunges.sum[,c(1,5,6,7)]),by = "group", all = TRUE)
results<-merge(results, AddData, by = "group", all = TRUE)
results$met.cost_lunge.V<-(0.5*Mc*(results$Vmax2.Vend2.median+2.4)/1000)/0.16


########
ggplot()+geom_boxplot(aes(tag,lunges_breath), data = Rates)
ggplot()+geom_line(aes(dive.start.sec,lunges, color = tag), data = AllTags6)
Rates
ggplot()+geom_point(aes( breaths_min, tag, color = feeding.), data = Rates)+geom_point(aes(tag, breaths_min), data = Rates2)

ggplot()+geom_boxplot(aes(tag,respr.rate_min, color = feeding.), data = AllTags6[AllTags6$feeding.=="y"&AllTags6$respr.rate_min>0,])+theme_classic()
ggplot()+geom_boxplot(aes(tag,lunge.rate_min, color = prey), data = AllTags6[AllTags6$feeding.=="y"&AllTags6$respr.rate_min>0,])+theme_classic()
ggplot()+geom_point(aes(lunges, breaths, color = feeding), data = Rates2[Rates2$tag == "mn13_108a",])

write.csv(summaryBy(.energy_m3+.prop_captured+.energy_gain ~group, FUN = median, data = EnergyAll),"C:/Users/Ellen/Desktop/results.csv",)



library("lme4", lib.loc="~/R/win-library/3.4")

mod<-lmer(.energy_gain~.biomass_m3+(1|group/tag), 
          data = EnergyAll[EnergyAll$group == "HerringTI" |
                             EnergyAll$group == "HerringSC" |
                             EnergyAll$group == "KrillSS" ,])

#do we need to account for variability in whale speed (does it correlate with prey density? does it differ by individual)
ggplot()+geom_point(aes(Sv_mean,max.speed, color = tag),data = Herring[Herring$tag  != "mn14_114a" & Herring$quality <1.1,])+geom_smooth(aes(Sv_mean,max.speed, color = tag),method = "lm", data = Herring[Herring$tag  != "mn14_114a" & Herring$quality <1.1,])

mod<-lm(max.speed~Sv_mean+tag, data = Herring[Herring$tag  != "mn14_114a" & Herring$quality <1.1,])
summary(mod)
#Figure 4 in Potvin et al 2012
names(df)
ggplot()+ geom_line(aes(t,Vw.t), data=df)+geom_line(aes(t,Vc.t), data=df)

#Figure 5 in Potvin et al 2012
ggplot()+ geom_line(aes(t,Fed.t), data=df)+geom_line(aes(t,Fsd.t), data=df)+geom_line(aes(t,Fsd.t+Fed.t), data=df)

#Plot water and whale accleration over time

ggplot()+geom_line(aes(t,ac.t), data = df)+geom_line(aes(t,aw.t), data = df)

###
ggplot()+geom_line(aes(t, Ac.t),lty = 2, df)+geom_line(aes(t, Ap.t), df)+theme_classic()+ylab("Area (m2)")
###
ggplot()+ geom_line(aes(t,Vlj.t), color= 2, data=df)+geom_line(aes(t,Vc.t), data=df)+theme_minimal()+ylab("m/s")
#####Adding acoustic data
SS2012<-read.csv("H:/Analysis/Chapter3/Acoustic Data/ALL2012_lunge_only.csv")
SS2012<-SS2012[SS2012$Sv_mean_linear>0.0000000001,]
#for krill

Energy_gain(Vp = 0.5, 
            Ddetect = 0.9, 
            df = df, 
            lungetype = "subsurface", 
            assim_eff = 0.92, 
            engulf_vol_m3 = 42,
            mass_ind = 0.11,
            energy_g = 3.52,
            Sv_mean = SS2012$Sv_mean,
            TS = -85,
            Prey = "krill")

CS2014<-read.csv("C:/Users/Ellen/Desktop/Analysis/Chapter 2/New folder/Schools_2.csv")
CS2014<-CS2014[c(1:8,15),]
#chum
summary(Energy_gain(Vp = 1.1, 
            Ddetect = 0.9, 
            df = df, 
            escape = 3, 
            assim_eff = 0.92, 
            engulf_vol_m3 = 42,
            mass_ind = 2.1,
            energy_g = 4.1,
            #g_m3 = 54.5,
            Sv_mean = CS2014$Sv_mean,
            TS = CS2014$Tracks_TS,
            Prey = "fish"))

#herring
TI2013<-read.csv("F:/Analysis/Chapter3/Acoustic Data/dataforellen_combined_regiononly_Sa_adj_AK2013_export_10mvert_100mhoriz_regions_turns_rem_cleaned-9999rem_groupsize10min1km_demersalschools_summarystats2.csv")
Acoustics<-TI2013
#for herring
assim_eff<-0.92
engulf_vol_m3 <- 42
mass_ind<-110 #grams
energy_ind<-5.5 #kJ/g
summary(Energy_gain(Vp = 4.5, 
            Ddetect = 1, 
            df = df, 
            escape = 1, 
            assim_eff = 0.92, 
            engulf_vol_m3 = 42,
            mass_ind = 110,
            energy_g = 5.5,
            Sv_mean = TI2013$Sv_mean,
            TS = TI2013$TS,
            #Height_mean = TI2013$Height_mean,
            Prey = "fish"))

Energy_gain.nodf(Energy_gain(Vp = 4.5, 
                             Ddetect = 1, 
                             df = df, 
                             escape = 1, 
                             assim_eff = 0.92, 
                             engulf_vol_m3 = 42,
                             mass_ind = 110,
                             energy_g = 5.5,
                             Sv_mean = TI2013$Sv_mean,
                             TS = TI2013$TS,
                             #Height_mean = TI2013$Height_mean,
                             Prey = "fish"))


both<-as.data.frame(rbind(cbind(herring.energy_gain, "herring"),cbind(krill.energy_gain, "krill")))
both<-as.data.frame(both)
chum<-data.frame(7265,"chum salmon")
names(chum)<-names(both)
both<-rbind(both,chum)
both[both$V2 == "chum salmon"& !is.na(both$V2),]$herring.energy_gain<-8463.151
both$herring.energy_gain<-as.numeric(as.character(both$herring.energy_gain))

p2<-ggplot()+geom_line(aes(herring.ind_m3,herring.energy_gain))
both<-both[-759,]
ggplot()+geom_boxplot(aes(V2,herring.energy_gain, fill = V2), data = both[both$herring.energy_gain>1 ,]) +
  scale_y_log10() + theme_classic()+ scale_fill_manual(values=c("orange",gg_color_hue(3)[3],"yellow"))+
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave("test.png", bg = "transparent")

mod<-lm(herring.energy_gain~V2, data = both)
  
ggplot()+geom_point(aes(herring.energy_gain, color = V2), data = both) + scale_y_log10() 


multiplot(p1,p2)
Energy_gain$krill



p1<-ggplot()+geom_line(aes(ind_m3,energy_gain), data = TI2013)+geom_line(aes(ind_m3,energy_gain), data = SS2012)
p1<-ggplot()+geom_line(aes(ind_m3,energy_gain), data = Acoustics)

multiplot(p1,p2,p3,p4)

p1<-ggplot()+geom_line(aes(t,Ac.t), data = df)+geom_line(aes(t,prey_void), data = df)+
  theme_minimal()+ scale_x_continuous(breaks = int_breaks)#+geom_ribbon(aes(ymin=prey_void,ymax=Ac.t), data=df, fill="blue", alpha="0.5")
p2<-ggplot()+geom_line(aes(t,Vc.t),data = df)+theme_minimal()+ scale_x_continuous(breaks = int_breaks)
p3<-ggplot()+geom_line(aes(t,THETAgape.t),data = df)+theme_minimal()+ scale_x_continuous(breaks = int_breaks)

multiplot(p1,p2,p3)
#Recreating goldbogen et al 2012 calcuations
V.0<-2.3 
V.i<-0.5
Mc<-35692
deltaKE<-0.5*Mc*(V.0^2 - V.i^2)/1000
2*deltaKE-4/7*deltaKE
2*deltaKE+4/7*deltaKE
