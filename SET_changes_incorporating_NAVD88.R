library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites")

# load group of files, generated from SET_summarizing.R
load("sets.group.R")

# make data frame of NAVD88 elevations of each SET platform
##############################
# NOT SURE THIS CALCULATION IS CORRECT
##############################
# So be wary of everything that follows until this is verified
##############################
# In the Excel spreadsheet, the calculation involved "SET_NAVD88 + Adapter_NAVD88 - 0.756", all subtracted from the original average of pin readings
# I don't know why 
NAVD88 <- read.csv("NAVD88.csv")
NAVD88 <- mutate(NAVD88, totalfactor = SET_NAVD88 - Adapter_NAVD88)


# subtract first reading at a platform from all subsequent readings
# split, apply, combine
pin_split <- split(pin_summary, pin_summary$site.platform) %>%
    lapply(., FUN = function(x) {mutate(x, "change" = mean - mean[1])})
pin_change <- do.call(rbind, pin_split) %>%
    mutate(., site = substr(site.platform, 1, 4))

# need to split upper and lower juncus
# JURO1-3 is lower
# 4-6 is upper
upper <- c("JURO-4", "JURO-5", "JURO-6")
lower <- c("JURO-1", "JURO-2", "JURO-3")
pin_change$site[pin_change$site.platform %in% upper] <- "UPJU"
pin_change$site[pin_change$site.platform %in% lower] <- "LOJU"

# order the sites as factors
pin_change$site <- factor(pin_change$site, levels = c("CLMA", "PANN", "UPJU", "LOJU", "SPAL"), ordered=TRUE)

# get NAVD88 corrected readings
# add NAVD88 adjustment to pin_change data frame by matching value from NAVD88 data frame 
# https://stackoverflow.com/questions/13492161/r-add-values-to-data-frame-matching-a-certain-criteria
pin_change$navdadj <- NAVD88$totalfactor[match(pin_change$site.platform, NAVD88$Site)]


# make the new column
# BEWARE - the units are different. turn this all into meters.
pin_change <- mutate(pin_change, meanadj = (mean/1000) + navdadj)


# big scatter plot of all sites
ggplot(pin_change, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site.platform), alpha=0.5, size=3) +
    theme_minimal() +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)")


# add a geom_abline for SLR of 3.5mm/yr
# that's the rate of SLR on Dauphin Island: https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?stnid=8735180
# following answer from: https://stackoverflow.com/questions/21946896/how-to-add-abline-in-ggplot2-with-x-axis-as-year

# to get that into rise per day, accounting for leap year:
# also convert it to m
slr_slope <- 3.5*4/(4*365+1)/1000


# and intercept, because time 0 is at 1970-01-01 and we want line to go through 0
# and MSL is 0.236
# MHW is 0.456
slr_int <- 0.236 - slr_slope*as.integer(pin_change[1,]$date)
mhw_int <- 0.456 - slr_slope*as.integer(pin_change[1,]$date)
mlw_int <- 0.039 - slr_slope*as.integer(pin_change[1,]$date)

# big scatter plot of all sites
png("SET elevations with MLW MSL and MHW.png", width=5, height=5, res=300, units="in")
ggplot(pin_change, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site.platform), alpha=0.5, size=3) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue") +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue") +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue") +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0,0.8)
dev.off()


png("SET elevations no CLMAJ with MLW and MSL.png", width=5, height=5, res=300, units="in")
ggplot(pin_change, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site.platform), alpha=0.5, size=3) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue") +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue") +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue") +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0,0.35)
dev.off()


############
# make it a little cleaner by summarizing by site
############

pins_bysite <- pin_change %>%
    group_by(site, date) %>%
    summarize(pin_ht_raw = mean(mean, na.rm=TRUE),
              pin_ht_adj = mean(meanadj, na.rm=TRUE))

############
# NOTE: I'm assuming these slopes are similar to the ones spit out from non-averaged data, but have NOT verified it
############

png("SET elevations by site with MLW and MSL.png", width=5, height=5, res=300, units="in")
ggplot(pins_bysite, aes(x=date, y=pin_ht_adj)) +
    geom_point(aes(col=site), alpha=0.5, size=3) +
    geom_smooth(method="lm", aes(group=site, col=site)) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue", lty=2) +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0, 0.8)
dev.off()    



png("SET elevations by site no CLMAJ with MLW and MSL.png", width=5, height=5, res=300, units="in")
ggplot(pins_bysite, aes(x=date, y=pin_ht_adj)) +
    geom_point(aes(col=site), alpha=0.3, size=3) +
    geom_smooth(method="lm", aes(group=site, col=site)) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue", lty=2) +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0, 0.35)
dev.off()


# facet
png("SET elevations by site faceted with MLW and MSL.png", width=8, height=5, res=300, units="in")
ggplot(pins_bysite, aes(x=date, y=pin_ht_adj)) +
    geom_point(aes(col=site), alpha=0.5, size=2) +
    facet_wrap(~site, nrow=2) +
    geom_smooth(method="lm", aes(group=site, col=site)) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue", lty=2) +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0, 0.8)
dev.off() 



# facet, including all 3 platforms from each site
png("SET elevations faceted with MLW and MSL.png", width=8, height=5, res=300, units="in")
ggplot(pin_change, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site), alpha=0.5, size=2) +
    facet_wrap(~site, nrow=2) +
    geom_smooth(method="lm", aes(col=site), lwd=1) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue", lty=2) +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0, 0.8)
dev.off() 

# exclude cladium
# facet, including all 3 platforms from each site
pin_change2 <- filter(pin_change, site != "CLMA")

png("SET elevations faceted no CLMAJ with MLW and MSL.png", width=8, height=5, res=300, units="in")
ggplot(pin_change2, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site), alpha=0.3, size=2) +
    facet_wrap(~site, nrow=2) +
    geom_smooth(method="lm", aes(col=site), lwd=1) +
    theme_minimal() +
    geom_abline(aes(slope = slr_slope, intercept = slr_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mhw_int), col="blue", lty=2) +
    geom_abline(aes(slope = slr_slope, intercept = mlw_int), col="blue", lty=2) +
    ggtitle("SETS through time relative to NAVD88") +
    ylab("elevation (m)") +
    ylim(0, 0.35)
dev.off() 
