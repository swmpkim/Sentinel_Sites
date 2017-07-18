library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites")

# load group of files, generated from SET_summarizing.R
load("sets.group.R")

# work with pin_summary df

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

change_bysite <- pin_change %>%
    group_by(site, date) %>%
    summarize(change = mean(change, na.rm=TRUE))

# big scatter plot of all sites
ggplot(pin_change, aes(x=date, y=change)) +
    geom_point(aes(col=site.platform), alpha=0.5, size=3) +
    theme_minimal() +
    ggtitle("Cumulative Surface Elevation Change") +
    ylab("change (mm)")


# add a geom_abline for SLR of 3.5mm/yr
# that's the rate of SLR on Dauphin Island: https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?stnid=8735180
# following answer from: https://stackoverflow.com/questions/21946896/how-to-add-abline-in-ggplot2-with-x-axis-as-year

# to get that into rise per day, accounting for leap year:
slr_slope <- 3.5*4/(4*365+1)
# update 7/18/17 - the above is equivalent to 3.5/365.25

# and intercept, because time 0 is at 1970-01-01 and we want line to go through 0
slr_int <- 0 - slr_slope*as.integer(pin_change[1,]$date)


# load the following to use stat_smooth_func in the plots to show the lm equations
# commented out though because the equations are in mm/day, not mm/yr, so they're not very useful
# but they do match the outputs from broom
# library(devtools)
# devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")


png("SET changes with SLR line and regressions faceted.png", width=5, height=8, res=300, units="in")
ggplot(pin_change, aes(x=date, y=change)) +
    geom_smooth(method="lm") +
    # stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
    facet_wrap(~site, ncol=1) +
    geom_abline(slope = slr_slope, intercept = slr_int, lty=2, lwd=1, alpha=0.5) +
    geom_point(aes(col=site.platform), alpha=0.7, size=3) +
    theme_minimal() +
    ggtitle("Cumulative Surface Elevation Change") +
    ylab("change (mm)")
dev.off()


png("SET changes with SLR line.png", width=6, height=3.5, res=300, units="in")
ggplot(pin_change, aes(x=date, y=change)) +
    geom_abline(slope = slr_slope, intercept = slr_int, lty=1, lwd=1) +
    geom_point(aes(col=site.platform), alpha=0.7, size=3) +
    theme_minimal() +
    ggtitle("Cumulative Surface Elevation Change") +
    ylab("change (mm)")
dev.off()


################################
# generate linear regression for each site.platform
# not very readable but it works
################################

# slope is per day because of how R sees dates
# based on: https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
models <- pin_change %>%
    group_by(site.platform) %>%  # could also group by site (CLMAJ/SPAL/etc) if desired
    do(mod = lm(change ~ date, data=.))

modelcoef <- tidy(models, mod, conf.int=TRUE, conf.level=0.95) # this gives the intercept, slope, and p values, among others; conf.int=TRUE, conf.level=0.95 gives the 95% confidence interval

# make a slope per year for date estimate; NAs for intercept
modelcoef$mm.yr <- ifelse(modelcoef$term == "date", round((modelcoef$estimate * 4 * 365 + 1)/4, 3), "NA")

# confidence intervals in mm/yr:
modelcoef$conf.low.yr <- ifelse(modelcoef$term == "date", round((modelcoef$conf.low * 4 * 365 + 1)/4, 3), "NA")
modelcoef$conf.high.yr <- ifelse(modelcoef$term == "date", round((modelcoef$conf.high * 4 * 365 + 1)/4, 3), "NA")

modelsummary <- glance(models, mod) # this gives r^2 and adjusted r^2, and p values, among others

# write.csv(modelsummary, "modelsummary_by_site-platform.csv", row.names=FALSE)
# write.csv(modelcoef, "modelcoef_by_site-platform.csv", row.names=FALSE)


#######################################
# regressions by site instead of by site.platform (n=3 within each site)
#######################################
models <- pin_change %>%
    group_by(site) %>%  
    do(mod = lm(change ~ date, data=.))

modelcoefbysite <- tidy(models, mod, conf.int=TRUE, conf.level=0.95) # this gives the intercept, slope, and p values, among others; conf.int=TRUE, conf.level=0.95 gives the 95% confidence interval

# make a slope per year for date estimate; NAs for intercept
# 7/18/2017 CORRECTED THESE CALCULATIONS
modelcoefbysite$mm.yr <- ifelse(modelcoefbysite$term == "date", round(modelcoefbysite$estimate * 365.25, 3), "NA")

# confidence intervals in mm/yr:
modelcoefbysite$conf.low.yr <- ifelse(modelcoefbysite$term == "date", round(modelcoefbysite$conf.low * 365.25, 3), "NA")
modelcoefbysite$conf.high.yr <- ifelse(modelcoefbysite$term == "date", round(modelcoefbysite$conf.high * 365.25, 3), "NA")

modelsummarybysite <- glance(models, mod) # this gives r^2 and adjusted r^2, and p values, among others

# write.csv(modelsummary, "modelsummary_by_site.csv", row.names=FALSE)
# write.csv(modelcoef, "modelcoef_by_site.csv", row.names=FALSE)


#########################################
# make graphs with regression lines generated above
#########################################

# pull out just intercept and slope from the modelcoefbysite data frame
regs <- modelcoefbysite %>%
    select(site, term, estimate) %>%
    spread(term, estimate)
names(regs) <- c("site", "intercept", "slope")

png("SET changes by site with regression.png", width=6, height=4.5, res=300, units="in")
ggplot(change_bysite, aes(x=date, y=change)) +
    geom_abline(slope = slr_slope, intercept = slr_int, lty=2, lwd=2, alpha=1) +
    geom_point(aes(col=site), alpha=0.3, size=2) +
    geom_smooth(method="lm", aes(group=site, color=site), alpha=1, lwd=1.5, se=FALSE) +
    # geom_abline(slope=regs$slope, intercept=regs$intercept, aes(col=site)) + FORTUNATELY this is the same as geom_smooth in the previous line, and this was generated from the 3-points per site per date data!
    theme_minimal() +
    ggtitle("Cumulative Surface Elevation Change") +
    ylab("change (mm)")
dev.off()


png("SET changes by site with SLR line faceted.png", width=5, height=5, res=300, units="in")
ggplot(change_bysite, aes(x=date, y=change)) +
    geom_abline(slope = slr_slope, intercept = slr_int, lty=2, lwd=2, alpha=1) +
    geom_smooth(method="lm", aes(group=site, color=site), alpha=1, lwd=1.5) +
    geom_point(aes(col=site), alpha=0.3, size=2) +
    facet_wrap(~site, ncol=2) +
    # geom_abline(slope=regs$slope, intercept=regs$intercept, aes(col=site)) +
    theme_minimal() +
    ggtitle("Cumulative Surface Elevation Change") +
    ylab("change (mm)")
dev.off()

