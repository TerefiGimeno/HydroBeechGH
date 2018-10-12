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
swc <- merge(swc, dryPots[,c('plantID','dryWeight','plant_weight','pot_plus_plate_weight','bulkDen')],
             by='plantID', all=T)
swc$GWC <- (swc$weight_g - swc$plant_weight-swc$pot_plus_plate_weight - swc$dryWeight)*100/swc$dryWeight
swc$VWC <- swc$GWC*swc$bulkDen
swc$nday <- as.numeric(swc$Date-as.Date("2018-05-15"))

modelSWC <- gnls(GWC ~ a*soilType*exp(-b*soilType*treatment*nday), data=swc, start=list(a=c(rep.int(50, 3)),b=c(rep.int(0.05, 6))),
                 param=list(a~soilType, b~soilType*treatment))

modelSWC <- gnls(GWC ~ a*exp(-b*treatment*nday), data=swc, start=list(b=c(0.05, 0.05)),
                 param=list(a~1, b~treatment))

plot(swc$VWC~swc$Date, col=as.factor(swc$soilType), pch=19)

model <- nlme::gnls(gc ~ g0+(1+xi/sqrt(Dmmol))*(Photo/(CO2S-42.75)), start=list(g0=c(rep.int(0,14)), xi=c(rep.int(3,14))),
              param=list(xi~spp*treatment, g0~spp*treatment), data=k)
