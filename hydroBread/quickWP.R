wp <- read.csv('HYDROBEECH_WP.csv')
wp$Date <- ymd(as.character(wp$Date_yyyymmdd))
pd <- subset(wp, time=='predawn')
k <- subset(pd, Date_yyyymmdd==20180524)
summary(aov(wpClean~treatment*soilType, data=k))
wpSumm <- as.data.frame(summarise(group_by(wp, Date, time, soilType, treatment),
                                  wpMean=mean.na(wpClean),wpSE=s.err(wpClean)))
e