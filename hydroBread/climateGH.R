source("hydroBread/readLibsHydro.R")
source("hydroBread/basicFun.R")
climate <- read.csv("hydroBdata/climateGH.csv")
climate <- climate[,c(1, 3:(ncol(climate)-1))]
climate$DateTime <- dmy_hm(as.character(climate$DateTime))
climate$Date <- dmy(as.character(climate$Date))
climate$VPD <- calcVPD(RH=climate$RH_Avg, temp=climate$Temp_Avg)
climate$DateHour <- nearestTimeStep(climate$DateTime, 60, "floor")
climateT <- tbl_df(climate)
climateHour <- as.data.frame(summarise(group_by(climateT, DateHour), Vpd=mean(VPD),
                                       Temp=mean(Temp_Avg), RH=mean(RH_Avg), PPFD=mean(PPFD_Avg)))
climateHour$nday <- as.numeric(climateHour$DateHour - ymd_hm(as.character("2018-05-15 23:00")))/(24*60*60)
climateDay <- as.data.frame(summarise(group_by(climateT, Date), Vpd=mean(VPD), VpdMax=max(VPD),
                                      Tmean=mean(Temp_Avg), Tmax=max(Temp_Avg), Tmin=min(Temp_Avg),
                                      RH=mean(RH_Avg), PPFD=sum(PPFD_Avg*10*60*1e-6)))
short <- subset(climateDay, Date>=as.Date("2018-05-01"))
# these are the values we will write in our methods
climateMethods <- data.frame(row.names=1:6)
climateMethods$variable <- c('Tmean','Tmin','Tmax','PPFD','VPD','VPDmax')
climateMethods$averages <- c(mean(short$Tmean), mean(short$Tmin), mean(short$Tmax), mean(short$PPFD),
                             mean(short$Vpd), mean(short$VpdMax))
climateMethods$serr <- c(s.err(short$Tmean), s.err(short$Tmin), s.err(short$Tmax), s.err(short$PPFD),
                             s.err(short$Vpd), s.err(short$VpdMax))
climateMethods$max <- c(max(short$Tmean), max(short$Tmin), max(short$Tmax), max(short$PPFD),
                        max(short$Vpd), max(short$VpdMax))
climateMethods$min <- c(min(short$Tmean), min(short$Tmin), min(short$Tmax), min(short$PPFD),
                        min(short$Vpd), min(short$VpdMax))