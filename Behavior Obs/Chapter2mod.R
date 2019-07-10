See Potvin et al 2012 in PLOSone Figure 3

##functions##
assignVec <- Vectorize("assign",c("x","value"))
deg2rad <- function(deg) {(deg * pi) / (180)}

##Read inputs##
setwd("~/Analysis/Behavior Obs")
var<-read.csv("Chapter2Model2.csv")
var$values<-as.numeric(as.character(var$values))
var$Rname<-as.character(var$Rname)
assignVec(var$Rname,var$values, envir = .GlobalEnv)

#Calculate engulf duration (equ 8)and total engulfment capacity (equn 9 and 10)
Tengulf<- (Xjd*Ljaw*sin(deg2rad(THETAgape))*THETAgape/(Hsync*Vc_0))*2  #EQ 8 

Mw_postTMJ<-Dwater*pi/3*(L0-Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 9 engulfed mass post TMJ
Mw_antTMJ<-Dwater*pi/3*(Ljaw)*(Xjd*Ljaw*sin(deg2rad(THETAgape)))*(0.5*Whead) #Eq 10
Mw<-Mw_postTMJ+Mw_antTMJ

Kopen
Kclose<- Kopen/(Kopen__Kclose)
Vc.t<-Vc_0
t<-dt
Vw.t<-0
Fww<-0 #  "Ocean to engulfed mass" drag
Vc.t
Mw.t<-0
Ac.t<-0
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
                 Fsd.t= numeric(1))
df[1,1]<-0
df[1,2]<-Ac.t
df[1,3]<-Mw.t
df[1,4]<-.Mw.t
df[1,5]<-Vc.t
df[1,6]<-Vw.t
df[1,7]<-ac.t
df[1,8]<-aw.t
df[1,9]<-THETAgape.t

n<-2
t<-dt

#Mouth opening stage
for (t in seq(0.01, Tengulf/2, dt)) {
    THETAgape.t<- Hsync*Vc_0/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*t #gape angle at time t if opening EQ6
    ifelse(t<=0.66*Topen, PHE<-1.6, PHE<-0 )
    Ac.t<-0.5*pi*Whead/2*(Xjd*Ljaw*sin(deg2rad(THETAgape.t))) # EQ11 mouth area in meters cubed
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
    
    df[n,1]<-t
    df[n,2]<-Ac.t
    df[n,3]<-Mw.t
    df[n,4]<-.Mw.t
    df[n,5]<-Vc.t
    df[n,6]<-Vw.t
    df[n,7]<-ac.t
    df[n,8]<-aw.t
    df[n,9]<-THETAgape.t
    df[n,10]<-Fed.t
    df[n,11]<-Fsd.t
    df
    n<-n+1
}

#Mouthclosing stage
for (t in seq(t+dt,Tengulf, dt)) {
  .THETAgape.t <-((THETAgape -(Hsync*Vc_0)/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*(t-Tengulf/2))-THETAgape.t)*100
  THETAgape.t<-THETAgape -(Hsync*Vc_0)/(sin(deg2rad(THETAgape))*Xjd*Ljaw)*(t-Tengulf/2) #gape angle at time t if closing EQ7
  
  PHE<-0
  Ac.t<-0.5*pi*Whead/2*(Xjd*Ljaw*sin(deg2rad(THETAgape.t))) # EQ11 mouth area in meters cubed
  .Mw_antTMJ.t<-(-Mw_antTMJ/THETAgape)*(.THETAgape.t) #EQ12b
  .Mw.t<-.Mw_antTMJ.t
  Mw.t<-Mw.t+.Mw.t*dt
  Fbc.t<-Kclose*4*Ac.t*Mw.t/(pi*Whead*Tengulf^2)# EQ17 Force applied to engulfed mass by buccal cavity walls
  Fed.t<-Fbc.t
  Kam<-0.2
  Fsd.t<-(Cd_close*Ac.t+Cd_body*Abody)*(0.5*Dwater*Vc.t^2)+ 0.2*Dwater*(pi/12)*L0*((Whead/2)+Ljaw)^2*ac.t #EQ21
  Fextthrust<- 4000*(Lbody-0.77)/(14.40-0.77)*(Lbody/14.40)^0.82 #Thrust plus bouyancy
  
  ac.t<-(Fextthrust-Fed.t-Fsd.t)/Mc
  Fww<-Fed.t*(1+(Mw.t/Mc))-Vw.t*.Mw_antTMJ.t #EQ27
  aw.t<- (Fbc.t-Fww-Vw.t*.Mw.t)/Mw.t
  Vw.t<-Vw.t+aw.t*dt
  Vc.t<- Vc.t+ac.t*dt
  
  df[n,1]<-t
  df[n,2]<-Ac.t
  df[n,3]<-Mw.t
  df[n,4]<-.Mw.t
  df[n,5]<-Vc.t
  df[n,6]<-Vw.t
  df[n,7]<-ac.t
  df[n,8]<-aw.t
  df[n,9]<-THETAgape.t
  df[n,10]<-Fed.t
  df[n,11]<-Fsd.t
  df
  n<-n+1
}
