library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


# setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites")

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



# generate linear regression for each site.platform
# not very readable but it works

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

write.csv(modelsummary, "modelsummary_by_site-platform.csv", row.names=FALSE)
write.csv(modelcoef, "modelcoef_by_site-platform.csv", row.names=FALSE)



# now instead of by site.platform, do it by site (n=3 within each site)
# generate linear regression for each site.platform
# not very readable but it works
# slope is per day because of how R sees dates
# based on: https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
models <- pin_change %>%
    group_by(site) %>%  
    do(mod = lm(change ~ date, data=.))

modelcoef <- tidy(models, mod, conf.int=TRUE, conf.level=0.95) # this gives the intercept, slope, and p values, among others; conf.int=TRUE, conf.level=0.95 gives the 95% confidence interval

# make a slope per year for date estimate; NAs for intercept
modelcoef$mm.yr <- ifelse(modelcoef$term == "date", round((modelcoef$estimate * 4 * 365 + 1)/4, 3), "NA")

# confidence intervals in mm/yr:
modelcoef$conf.low.yr <- ifelse(modelcoef$term == "date", round((modelcoef$conf.low * 4 * 365 + 1)/4, 3), "NA")
modelcoef$conf.high.yr <- ifelse(modelcoef$term == "date", round((modelcoef$conf.high * 4 * 365 + 1)/4, 3), "NA")

modelsummary <- glance(models, mod) # this gives r^2 and adjusted r^2, and p values, among others

write.csv(modelsummary, "modelsummary_by_site.csv", row.names=FALSE)
write.csv(modelcoef, "modelcoef_by_site.csv", row.names=FALSE)




# this is working
# test.dat2 <- filter(pin_change, site.platform == "JURO-1")
# test.mod.2 <- lm(change ~ date, data=test.dat2)
# summary(test.mod.2)
# slopeperyear <- (test.mod.2$coefficients[2]*(4*365+1))/4
# slopeperyear
# 
# # residual sum of squares is, from https://stackoverflow.com/questions/15254543/obtain-residual-standard-errors-of-an-mlm-object-returned-by-lm:
# resSS <- sum(test.mod.2$residuals^2)
# # not 100% sure this matches the definition of res SS on p. 269 of Zar's Biostatistical Analysis, 2nd ed.
# # this matches "deviance" in the modelsummary dataframe
# 
# confint(test.mod.2, level=0.95) # 95% confidence interval for estimates (date slope)
# # anova table also gives residual sum of squares:
# # anova(test.mod.2)

# plot diagnostics
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
# plot(test.mod.2)


# ggplot(test.dat2, aes(x=date, y=change)) +
#     geom_abline(slope = slr_slope, intercept = slr_int, lty=1, lwd=1) +
#     geom_abline(slope = test.mod.2$coefficients[2], intercept = test.mod.2$coefficients[1], lty=2, col="red", lwd=2) +
#     geom_point(aes(col=site.platform), alpha=0.7, size=3) +
#     ylim(-40, 45) +
#     theme_minimal() +
#     ggtitle("Cumulative Surface Elevation Change") +
#     ylab("change (mm)")
