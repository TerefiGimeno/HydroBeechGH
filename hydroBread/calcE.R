source("hydroBread/climateGH.R")
swc <- read.csv("hydroBdata/potWeights.csv")
swc$Date <- ymd(as.character(swc$Date_yyyymmdd))
dryPots <- read.csv("hydroBdata/finalSoilWeight.csv")
dryPots$pot_plus_plate_weight <- ifelse(is.na(dryPots$pot_plus_plate_weight), dryPots$plate_weight+dryPots$pot_weight,
                                        dryPots$pot_plus_plate_weight)
dryPots$finalSWC <- (dryPots$Soil_wet_weight-dryPots$Soil_dry_weight)*100/(dryPots$Soil_dry_weight-dryPots$paper_bag_g)
dryPots$distance_soil_to_pot_border_cm <- ifelse(is.na(dryPots$distance_soil_to_pot_border_cm),
                                                 mean(dryPots$distance_soil_to_pot_border_cm, na.rm=T),
                                                 dryPots$distance_soil_to_pot_border_cm)
dryPots$soilVolume <- ((2-dryPots$distance_soil_to_pot_border_cm*0.1)/3)*(1.5+1.1+sqrt(1.5*1.1))
dryPots$dryWeight <- ifelse(is.na(dryPots$cryoSampleSoil_wetWeight_g), dryPots$Soil_dry_weight-dryPots$paper_bag_g,
                            dryPots$Soil_dry_weight - dryPots$paper_bag_g + 
                              dryPots$cryoSampleSoil_wetWeight_g -
                              dryPots$cryoSampleSoil_wetWeight_g*0.01*dryPots$finalSWC)
dryPots$bulkDen <- dryPots$dryWeight*0.001/dryPots$soilVolume
leafArea <- read.csv("hydroBdata/leafArea.csv")
swc <- merge(swc, dryPots[,c('plantID','dryWeight','plant_weight','pot_plus_plate_weight','bulkDen')],
             by='plantID', all=T)
swc <- merge(swc, leafArea[,c('plantID','totalLeafArea_cm2')], by='plantID', all=T)
swc$GWC <- (swc$weight_g - swc$plant_weight-swc$pot_plus_plate_weight - swc$dryWeight)*100/swc$dryWeight
swc$VWC <- swc$GWC*swc$bulkDen
swc$nday <- as.numeric(swc$Date-as.Date("2018-05-15"))
#this doesn't work and I don't know why
modelGWC <- gnls(GWC ~ a*soilType*exp(-b*treatment*soilType*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 param=list(a~soilType, b~treatment*soilType))
modelVWC <- gnls(VWC ~ a*soilType*exp(-b*treatment*soilType*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 param=list(a~soilType, b~treatment*soilType))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="A" & treatment=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="A" & treatment=='drought'), start=list(a=50, b=0.05)))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="B" & treatment=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="B" & treatment=='drought'), start=list(a=50, b=0.05)))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="C" & treatment=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, soilType=="C" & treatment=='drought'), start=list(a=50, b=0.05)))

plot(swc$VWC~swc$Date, col=as.factor(swc$soilType), pch=19)

porom <- read.csv("hydroBdata/conductancePorometers.csv")
p

