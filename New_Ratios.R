library(dplyr)
library(tidyverse)

bat = read.csv(file.choose())

bat$BB.K = round(bat$BB / bat$SO, 2)

bat$hardhits.SO = round(bat$HardHit / bat$SO, 2)
SO.hardhits = round(bat$SO / bat$HardHit, 2)


hist(bat$BB.K)

# avg_hh.SO = round(sum(bat$HardHit) / sum(bat$SO), 3)

test = data.frame(Name = bat$Name,
                  PA = bat$PA,
                  HHits.SO = bat$hardhits.SO,
                  HardHits = bat$HardHit,
                  SO = bat$SO,
                  ev = bat$EV,
                  maxEV = bat$maxEV,
                  Barrel. = bat$Barrel.)
# Don't buy Kevin Newman. Can you up ev and maxEV

ggplot(test, aes(ev, hardhits.SO)) + geom_point()


ggplot(test, aes(maxEV, hardhits.SO)) + geom_point()

ggplot(test, aes(ev, maxEV)) + geom_point()

test2 = subset(test, ev >= 87 & hardhits.SO >= 2)

maxEV_109 = subset(test, maxEV >= 109 & maxEV <= 110)

hist(hardhits.SO,
     main = "Hard-Hits/SO for All Hitters", 
     xlab = "Hard-Hits/SO",           
     ylab = "Frequency",
     col = 4,
     breaks = 40) + 
  abline(v = avg_hh.SO, col = "black", lwd = 3)

###-----------------------------------------------------------------------------

avg_SO.hh = round(sum(bat$SO) / sum(bat$HardHit), 3)

hist(SO.hardhits,
     main = "SO/Hard-Hits for All Hitters", 
     xlab = "SO/Hard-Hits",           
     ylab = "Frequency",
     col = 5,
     breaks = 20) + abline(v = avg_SO.hh, col = "black", lwd = 3)


###-----------------------------------------------------------------------------

bat$HardHit. = as.character(bat$HardHit.)
bat$HardHit. = gsub("%", "", as.character(bat$HardHit.))
bat$HardHit. = as.numeric(bat$HardHit.)

bat$Barrel. = as.character(bat$Barrel.)
bat$Barrel. = gsub("%", "", as.character(bat$Barrel.))
bat$Barrel. = as.numeric(bat$Barrel.)


###-----------------------------------------------------------------------------

bat$newNum1 = (bat$HardHit + bat$BB) - bat$SO # (Hard-Hits + BB) - SO
bat$newRatio1 = round((bat$HardHit + bat$BB) / bat$SO, 2) # (Hard-Hits + BB) / SO


hist(bat$newNum1,
     main = "(Hard-Hits + BB) - SO for All Hitters", 
     xlab = "(H-Hits + BB) - K",           
     ylab = "Frequency",
     col = 3,
     breaks = 25) +
  abline(v = round(((sum(bat$BB) + sum(bat$HardHit)) - sum(bat$SO)) / nrow(bat), 3), col = "black", lwd = 3)


hist(bat$newRatio1,
     main = "(Hard-Hits + BB) / SO for All Hitters", 
     xlab = "(H-Hits + BB) / K",           
     ylab = "Frequency",
     col = 4,
     breaks = 25) +
  abline(v = round((sum(bat$BB) + sum(bat$HardHit)) / sum(bat$SO), 3), col = "black", lwd = 3)

