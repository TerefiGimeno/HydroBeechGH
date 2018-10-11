source("hydroBread/climateGH.R")
swc <- read.csv("hydroBdata/potWeights.csv")
swc$Date <- ymd(as.character(swc$Date_yyyymmdd))
dryPots <- read.csv("hydroBdata/finalSoilWeight.csv")
dryPots$pot_plus_plate_weight <- ifelse(is.na(dryPots$pot_plus_plate_weight), dryPots$plate_weight+dryPots$pot_weight,
                                        dryPots$pot_plus_plate_weight)
dryPots$finalSWC <- (dryPots$Soil_wet_weight-dryPots$Soil_dry_weight)*100/dryPots$Soil_dry_weight
dryPots$distance_soil_to_pot_border_cm <- ifelse(is.na(dryPots$distance_soil_to_pot_border_cm),
                                                 mean(dryPots$distance_soil_to_pot_border_cm, na.rm=T),
                                                 dryPots$distance_soil_to_pot_border_cm)
dryPots$soilVolume <- ((1.5-dryPots$distance_soil_to_pot_border_cm*0.1)/3)*(1.5+1.25+sqrt(1.5*1.25))
dryPots$dryWeight <- ifelse(is.na(dryPots$cryoSampleSoil_wetWeight_g), dryPots$Soil_dry_weight-dryPots$paper_bag_g,
                            dryPots$Soil_dry_weight-dryPots$paper_bag_g - dryPots$paper_bag_g + 
                              dryPots$cryoSampleSoil_wetWeight_g -
                              dryPots$cryoSampleSoil_wetWeight_g*0.01*dryPots$finalSWC)
dryPots$bulkDen <- dryPots$Soil_dry_weight*0.001/dryPots$soilVolume
swc <- merge(swc, dryPots[,c('plantID','dryWeight','plant_weight','pot_plus_plate_weight','bulkDen')],
             by='plantID', all=T)
swc$GWC <- (swc$weight_g-swc$plant_weight-swc$pot_plus_plate_weight-swc$dryWeight)*100/swc$dryWeight
swc$VWC <- swc$GWC*swc$bulkDen
swc$nday <- as.numeric(swc$Date-as.Date("2018-05-15"))

modelSWC <- nls(GWC ~ a*soilType*exp(-b*soilType*treatment*nday), data=swc, star=list(a=50,b=0.05))

plot(swc$VWC~swc$Date, col=as.factor(swc$soilType), pch=19)
