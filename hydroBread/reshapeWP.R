wp <- read.csv("hydroBdata/waterPotential.csv")
wpPd <- subset(wp, time=='predawn')[,c('Soil','Plant','Treat','Date','WP_Mpa','notes')]
colnames(wpPd)[c(ncol(wpPd)-1, ncol(wpPd))] <- c("pd_wp", "notes_pd_wp")
wpMd <- subset(wp, time=="midday")[,c('Plant','Date','WP_Mpa','notes')]
colnames(wpMd)[c(ncol(wpMd)-1, ncol(wpMd))] <- c("md_wp", "notes_md_wp")
wpShort <- merge(wpPd, wpMd, by=c('Plant','Date'), all=T)
write.csv(wpShort, file='hydroBoutput/wpWide.csv', row.names=F)