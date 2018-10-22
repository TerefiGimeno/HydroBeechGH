wp <- read.csv('hydroBoutput/wpWide.csv')
wp$Date <-ymd(as.character(wp$Date))
wp$nday <- as.numeric(wp$Date-as.Date("2018-05-15"))
wpSumm <- doBy::summaryBy(pd_wp + md_wp ~ Date + Soil + Treat, FUN=c(mean.na, s.err, length),
                          data=wp)
