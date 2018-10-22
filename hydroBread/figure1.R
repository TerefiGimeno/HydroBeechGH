source("hydroBread/calcE.R")
source("hydroBread/quickWP.R")
wpo <- subset(wpSumm, Treat=='drought' | Treat == 'control')
windows(10,12)
swcSumm <- doBy::summaryBy(VWC ~ Soil + Treat + nday, data=swc, FUN=c(mean.na, s.err, length), keep.names = T)
swcSumm$useless <- as.factor(paste0(swcSumm$Soil, '_', swcSumm$Treat))
empty <- list()
for (i in 1:length(levels(swcSumm$useless))){
  empty[[i]] <- subset(swcSumm, useless==levels(swcSumm$useless)[i])
}
par(mfrow=c(2,1), las=1)
par(mar=c(0,6,2,2))
plot(empty[[1]][,'nday'], empty[[1]][,'VWC.mean.na'], lty=2, col='deeppink', lwd=2, ylim=c(0,60), ylab='VWC (%)',
      xlab='Time (days)',cex.lab=1.5, type='l')
lines(empty[[2]][,'nday'], empty[[2]][,'VWC.mean.na'], col='deeppink', lwd=2)
lines(empty[[3]][,'nday'], empty[[3]][,'VWC.mean.na'], col='chocolate4', lwd=2, lty=2)
lines(empty[[4]][,'nday'], empty[[4]][,'VWC.mean.na'], col='chocolate4', lwd=2)
lines(empty[[5]][,'nday'], empty[[5]][,'VWC.mean.na'], col='cornflowerblue', lwd=2, lty=2)
lines(empty[[6]][,'nday'], empty[[6]][,'VWC.mean.na'], col='cornflowerblue', lwd=2)
Hmisc::errbar(empty[[1]][,'nday'], empty[[1]][,'VWC.mean.na'], empty[[1]][,'VWC.mean.na'] + empty[[1]][,'VWC.s.err'],
              empty[[1]][,'VWC.mean.na'] - empty[[1]][,'VWC.s.err'], pch=1,  col='deeppink',  cex=1.2, add=T)
Hmisc::errbar(empty[[2]][,'nday'], empty[[2]][,'VWC.mean.na'], empty[[2]][,'VWC.mean.na'] + empty[[2]][,'VWC.s.err'],
              empty[[2]][,'VWC.mean.na'] - empty[[2]][,'VWC.s.err'], pch=19, col='deeppink', cex=1.2, add=T)
Hmisc::errbar(empty[[3]][,'nday'], empty[[3]][,'VWC.mean.na'], empty[[3]][,'VWC.mean.na'] + empty[[3]][,'VWC.s.err'],
              empty[[3]][,'VWC.mean.na'] - empty[[3]][,'VWC.s.err'], pch=1,  col='chocolate4',  cex=1.2, add=T)
Hmisc::errbar(empty[[4]][,'nday'], empty[[4]][,'VWC.mean.na'], empty[[4]][,'VWC.mean.na'] + empty[[4]][,'VWC.s.err'],
              empty[[4]][,'VWC.mean.na'] - empty[[4]][,'VWC.s.err'], pch=19, col='chocolate4', cex=1.2, add=T)
Hmisc::errbar(empty[[5]][,'nday'], empty[[5]][,'VWC.mean.na'], empty[[5]][,'VWC.mean.na'] + empty[[5]][,'VWC.s.err'],
              empty[[5]][,'VWC.mean.na'] - empty[[5]][,'VWC.s.err'], pch=1,  col='cornflowerblue',  cex=1.2, add=T)
Hmisc::errbar(empty[[6]][,'nday'], empty[[6]][,'VWC.mean.na'], empty[[6]][,'VWC.mean.na'] + empty[[6]][,'VWC.s.err'],
              empty[[6]][,'VWC.mean.na'] - empty[[6]][,'VWC.s.err'], pch=19, col='cornflowerblue', cex=1.2, add=T)
legend('topright', pch=c(1,19), lty = c(1,2), legend=c('Control','Drought'), bty='n')
legend('bottomleft', lty=1, col=c('deeppink','chocolate4','cornflowerblue'),
       legend=c('Sand', 'Sand & Rock', 'Sandy Clay Loam'), bty='n', lwd=2)
