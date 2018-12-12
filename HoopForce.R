library(ggplot2)
library(plotly)
library(reshape2)
library(openxlsx)
library (xlsx)
library(grid)
library(gridExtra)
library(readxl)
library(lubridate)



#Pressure Readings


# Balloon 1

df <- read_excel('11008_01.xlsx', col_types = "text")
as.POSIXct(df$Time, format="%d/%m/%Y %H:%M:%S", tz="CET")




GetPressData <- function (path)
{

path <- path
data <- read.csv2(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
names(data) <- c("Time", "Pressure")
data$Pressure <- as.numeric(data$Pressure)
data$Time2 <- substr(data$Time, 12, 23)
data$Time3 <- hms(data$Time2)
data$Time4 <- period_to_seconds(data$Time3)
data$Duration <- data$Time4

j<-1
while(j<=length(data$Time))
{
  data$Duration[j]<-(data$Time4[j] - data$Time4[1])
  j<- j+1
}

data <- data[,c(2,6)]
names(data) <- c("Pressure", "Time")

return(data)

}

Press1 <- GetPressData("11008_01.csv")
Press2 <- GetPressData("11008_02.csv")
Press3 <- GetPressData("11008_03.csv")
Press4 <- GetPressData("11008_04.csv")
Press5 <- GetPressData("11008_05.csv")



#Force Readings

#Force <- read.csv("Force.csv", header = TRUE, sep = ",")

#Force1 <- Force[, c(1,2)]
#names(Force1) <- c("Time", "Force")

#Force2 <- Force[, c(1,3)]
#names(Force2) <- c("Time", "Force")

#Force3 <- Force[, c(1,4)]
#names(Force3) <- c("Time", "Force")

#Force4 <- Force[, c(1,5)]
#names(Force4) <- c("Time", "Force")

#Force5 <- Force[, c(1,6)]
#names(Force5) <- c("Time", "Force")


#Force1 <- read.xlsx2("Force.xlsx", sheetIndex = 3, startRow = 2, header = FALSE, stringsAsFactors = FALSE)
#Force1 <- Force1[,c(1,3)]
#names(Force1) <- c("Time", "Force")
#Force1$Force <- as.numeric(Force1$Force)
#Force1$Time <- as.numeric(Force1$Time)


GetForceData <- function(n){
  data <- read.xlsx2("Force.xlsx", sheetIndex = n, startRow = 2, header = FALSE, stringsAsFactors = FALSE)
  data <- data[,c(1,3)]
  names(data) <- c("Time", "Force")
  data$Force <- as.numeric(data$Force)
  data$Time <- as.numeric(data$Time)
  return(data)
}

Force1 <- GetForceData(3)
Force2 <- GetForceData(4)
Force3 <- GetForceData(5)
Force4 <- GetForceData(6)
Force5 <- GetForceData(7)


#Volume Readings

Volume <- read.csv("Volume.csv", header = TRUE, sep = ",")

Vol1 <- Volume[which(Volume$Balloon == 1),c(1:2)]

Vol2 <- Volume[which(Volume$Balloon == 2),c(1:2)]

Vol3 <- Volume[which(Volume$Balloon == 3),c(1:2)]

Vol4 <- Volume[which(Volume$Balloon == 4),c(1:2)]

Vol5 <- Volume[which(Volume$Balloon == 5),c(1:2)]




f1 <- ggplot(data = Force1, aes(x= Time, y = Force)) + geom_line()

ggplotly(f1)

f2 <- ggplot(data = Force2, aes(x= Time, y = Force)) + geom_line()

ggplotly(f2)

f3 <- ggplot(data = Force3, aes(x= Time, y = Force)) + geom_line()

ggplotly(f3)

f4 <- ggplot(data = Force4, aes(x= Time, y = Force)) + geom_line()

ggplotly(f4)

f5 <- ggplot(data = Force5, aes(x= Time, y = Force)) + geom_line()

ggplotly(f5)





p1 <- ggplot(data = Press1, aes(x = Time, y = Pressure)) + geom_line()

ggplotly(p1)

p2 <- ggplot(data = Press2, aes(x = Time, y = Pressure)) + geom_line()

ggplotly(p2)

p3 <- ggplot(data = Press3, aes(x = Time, y = Pressure)) + geom_line()

ggplotly(p3)

p4 <- ggplot(data = Press4, aes(x = Time, y = Pressure)) + geom_line()

ggplotly(p4)  

p5<- ggplot(data = Press5, aes(x = Time, y = Pressure)) + geom_line()

ggplotly(p5)  





setT<- function(data, x)
{
  
  data <- data
  
  data$Tminus <- data$Time
  data$Tminus <- data$Tminus - x 
  
  return(data)
  
}


P1 <-  setT(Press1,1073.976)
P2 <- setT(Press2, 135.99)
P3 <- setT(Press3,440.51)
P4 <- setT(Press4, 241.93)
P5 <- setT(Press5, 142.357)

F1 <- setT(Force1, 58.6)
F2 <- setT(Force2,57.4)
F3 <- setT(Force3, 52.89)
F4 <- setT(Force4, 39.89)
F5 <- setT(Force5, 49.10)

V1 <- setT(Vol1, 64)
V2 <- setT(Vol2, 65)
V3 <- setT(Vol3, 59)
V4 <- setT(Vol4, 55)
V5 <- setT(Vol5, 64)







MakePlots <- function (Pressure,Force,Volume, start, end)
{
  
  x <- Volume[which(Volume$Volume == 30),3]
  
plot1 <- ggplot(data = Pressure, aes(x=Tminus, y = Pressure)) + geom_line()
plot1 <- plot1 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
  scale_y_continuous(breaks = seq(0,6,2), limits = c(0,6)) +
  geom_vline(xintercept=x, colour="black")+
  geom_vline(xintercept=0, colour="black")

plot1

plot2 <- ggplot(data = Force, aes(x=Tminus, y = Force)) + geom_line()
plot2 <- plot2 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
  scale_y_continuous(breaks = seq(-5,40,10), limits = c(-5,40)) +
  geom_vline(xintercept=x, colour="black")+
  geom_vline(xintercept=0, colour="black")

plot2

plot3 <- ggplot(data = Volume, aes(x=Tminus, y = Volume)) + geom_line()
plot3 <- plot3 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
  scale_y_continuous(breaks = seq(0,50,10), limits = c(0,50))+
  geom_vline(xintercept=x, colour="black")+
  geom_vline(xintercept=0, colour="black")

plot3


gp1<- ggplot_gtable(ggplot_build(plot1))
gp2<- ggplot_gtable(ggplot_build(plot2))
gp3<- ggplot_gtable(ggplot_build(plot3))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3],gp3$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth

g1 <- grid.arrange(gp1, gp2, gp3)

g1
}

Device1 <- MakePlots(P1,F1,V1,-60,3)
Device2 <- MakePlots(P2,F2,V2,-55,3)
Device3 <- MakePlots(P3,F3,V3,-50,3)
Device4 <- MakePlots(P4,F4,V4,-40,3)
Device5 <- MakePlots(P5,F5,V5,-40,3)



addDeviceNum <- function(data, n){

  data$Device <- ""
  data$Device <- n
  return(data)
  
}


VV1 <- addDeviceNum(V1, 1)
VV2 <- addDeviceNum(V2, 2)
VV3 <- addDeviceNum(V3, 3)
VV4 <- addDeviceNum(V4, 4)
VV5 <- addDeviceNum(V5, 5)

VComb <- rbind(VV1,VV2,VV3,VV4,VV5)
VComb$Device<-as.factor(VComb$Device)

FF1 <- addDeviceNum(F1, 1)
FF2 <- addDeviceNum(F2, 2)
FF3 <- addDeviceNum(F3, 3)
FF4 <- addDeviceNum(F4, 4)
FF5 <- addDeviceNum(F5, 5)

FComb <- rbind(FF1,FF2,FF3,FF4,FF5)
FComb$Device<-as.factor(FComb$Device)

PP1 <- addDeviceNum(P1, 1)
PP2 <- addDeviceNum(P2, 2)
PP3 <- addDeviceNum(P3, 3)
PP4 <- addDeviceNum(P4, 4)
PP5 <- addDeviceNum(P5, 5)

PComb <- rbind(PP1,PP2,PP3,PP4,PP5)
PComb$Device<-as.factor(PComb$Device)

CombinedPlots <- function (Pressure,Force,Volume)
{
  
  Pressure <- PComb
  Force <- FComb
  Volume <- VComb
  #x <- Volume[which(Volume$Volume == 30),3]
  start <- -50
  end <- 3
  
  plot1 <- ggplot(data = Pressure, aes(x=Tminus, y = Pressure, col = Device)) + geom_line()
  plot1 <- plot1 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
    scale_y_continuous(breaks = seq(-1,6,2), limits = c(-1,6)) +
    #geom_vline(xintercept=x, colour="black")+
    geom_vline(xintercept=0, colour="black")
  
  plot1
  
  plot2 <- ggplot(data = Force, aes(x=Tminus, y = Force, col = Device)) + geom_line()
  plot2 <- plot2 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
    scale_y_continuous(breaks = seq(-5,40,10), limits = c(-5,40)) +
    #geom_vline(xintercept=x, colour="black")+
    geom_vline(xintercept=0, colour="black")
  
  plot2
  
  plot3 <- ggplot(data = Volume, aes(x=Tminus, y = Volume, col = Device)) + geom_line()
  plot3 <- plot3 +scale_x_continuous(breaks = seq(start,end,10), limits = c(start,end))+
    scale_y_continuous(breaks = seq(0,50,10), limits = c(0,50))+
    #geom_vline(xintercept=x, colour="black")+
    geom_vline(xintercept=0, colour="black")
  
  plot3
  
  
  gp1<- ggplot_gtable(ggplot_build(plot1))
  gp2<- ggplot_gtable(ggplot_build(plot2))
  gp3<- ggplot_gtable(ggplot_build(plot3))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3],gp3$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  gp3$widths[2:3] <- maxWidth
  
  
  
  g1 <- grid.arrange(gp1, gp2, gp3)
  
  g1
}

CombinedPlots(PComb,FComb,VComb)




## Generating Force Vs Pressure Graphs

addForce <- function (Vorigin,Forigin,Device)
{
Vorigin$Force <- NULL
Vorigin$Device <- ""
Vorigin$Device <- Device

j<-1
while(j<=length(Vorigin$Time))
{
  
  Vorigin$Force[j] <- Forigin$Force[which.min(abs(Forigin$Tminus-Vorigin$Tminus[j]))]
 
  j<- j+1
}

return(Vorigin)

}

VF1 <- addForce(V1,F1,"1")
VF2 <- addForce(V2,F2,"2")
VF3 <- addForce(V3,F3,"3")
VF4 <- addForce(V4,F4,"4")
VF5 <- addForce(V5,F5,"5")


Combined <- rbind(VF1,VF2,VF3,VF4,VF5)

cPlot <- ggplot(data = Combined, aes(x= Volume, y = Force, col = Device)) + geom_line()
cPlot <- cPlot +scale_x_continuous(breaks = seq(0,50,10), limits = c(0,50))+
  scale_y_continuous(breaks = seq(0,45,20), limits = c(0,45))

cPlot

ggplotly(cPlot)



addPressure <- function (Vorigin,Porigin,Device)
{
  Vorigin$Pressure <- NULL
  Vorigin$Device <- ""
  Vorigin$Device <- Device
  
  j<-1
  while(j<=length(Vorigin$Time))
  {
    
    Vorigin$Pressure[j] <- Porigin$Pressure[which.min(abs(Porigin$Tminus-Vorigin$Tminus[j]))]
    
    j<- j+1
  }
  
  return(Vorigin)
  
}


VP1 <- addPressure(V1,P1,"1")
VP2 <- addPressure(V2,P2,"2")
VP3 <- addPressure(V3,P3,"3")
VP4 <- addPressure(V4,P4,"4")
VP5 <- addPressure(V5,P5,"5")

CombinedVP <- rbind(VP1,VP2,VP3,VP4,VP5)

cPlot2 <- ggplot(data = CombinedVP, aes(x= Volume, y = Pressure, col = Device)) + geom_line()
cPlot2 <- cPlot2 +scale_x_continuous(breaks = seq(0,50,10), limits = c(0,50))+
  scale_y_continuous(breaks = seq(0,4,1), limits = c(0,4))

cPlot2


