source("hydroBread/climateGH.R")
swc <- read.csv("hydroBdata/potWeights.csv")
swc$Date <- ymd(as.character(swc$Date))
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
swc <- merge(swc, dryPots[,c('plant','dryWeight','plant_weight','pot_plus_plate_weight','bulkDen')],
             by='plant', all=T)
swc <- merge(swc, leafArea[,c('plant','totalLeafArea_cm2')], by='plant', all=T)
swc$GWC <- (swc$weight_g - swc$plant_weight-swc$pot_plus_plate_weight - swc$dryWeight)*100/swc$dryWeight
swc$VWC <- swc$GWC*swc$bulkDen
swc$nday <- as.numeric(swc$Date-as.Date("2018-05-15"))

#analysis of co-variance
modelGWC <- lm(log(GWC)~nday*Treat*Soil, data=swc)
summary(aov(log(GWC)~nday*Treat*Soil, data=swc))
modelVWC <- lm(log(VWC)~nday*Treat*Soil, data=swc)
summary(aov(log(VWC)~nday*Treat*Soil, data=swc))

#one-way ANOVA of maximum and minimum water holding capacity
summary(aov(GWC~Soil, data=subset(swc, nday==1)))
TukeyHSD(aov(GWC~Soil, data=subset(swc, nday==1)))
summary(aov(VWC~Soil, data=subset(swc, nday==1)))
TukeyHSD(aov(VWC~Soil, data=subset(swc, nday==1)))
summary(aov(GWC~Soil, data=subset(swc, nday==36 & Treat=="drought")))
TukeyHSD(aov(GWC~Soil, data=subset(swc, nday==36 & Treat=="drought")))
summary(aov(VWC~Soil, data=subset(swc, nday==36 & Treat=="drought")))
TukeyHSD(aov(VWC~Soil, data=subset(swc, nday==36 & Treat=="drought")))
one <- summaryBy(GWC + VWC ~ Soil, data=subset(swc, nday==1), FUN=c(mean, s.err, length))
one$nday <- c(rep(1, times=3))
two <- summaryBy(GWC + VWC ~ Soil, data=subset(swc, nday==36 & Treat=="drought"), FUN=c(mean, s.err, length))
two$nday <- c(rep(36, times=3))
write.csv(rbind(one, two), file="hydroBoutput/summarySWC.csv", row.names=F)

#this doesn't work and I don't know why
modelGWC <- gnls(GWC ~ a*Soil*exp(-b*Treat*Soil*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 param=list(a~Soil, b~Treat*Soil))
modelVWC <- gnls(VWC ~ a*Soil*exp(-b*Treat*Soil*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 param=list(a~Soil, b~Treat*Soil))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="A" & Treat=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="A" & Treat=='drought'), start=list(a=50, b=0.05)))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="B" & Treat=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="B" & Treat=='drought'), start=list(a=50, b=0.05)))
summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="C" & Treat=='drought'), start=list(a=50, b=0.05)))
confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="C" & Treat=='drought'), start=list(a=50, b=0.05)))

plot(log(swc$GWC)~swc$Date, col=as.factor(swc$Soil), pch=19)
windows(12,8)
par(mfrow=c(2,1))
plot(swc$VWC~swc$nday, col=as.factor(swc$Soil), pch=19, ylab="VWC (%)", xlab="Time (days)", cex.lab=1.5)


porom <- read.csv("hydroBdata/conductancePorometers.csv")
