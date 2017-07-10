library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites")

# read in SETs.csv and make it a long data set
sets <- read.csv("SETs.csv") %>%
    gather(key = date, value = pin_height, -site, -site.platform, -site.platform.position, -platform_number, -arm_position, -pin_number)

# remove the 'X' before the date and put it in date format
sets$date <- sub('.', '', sets$date) %>%
    as.Date(sets$date, format = "%m.%d.%Y")

# load data file (processed from SET_processing.R)
# load("sets.longfile.R")


# group by platform and date, then calculate mean and standard deviation
sets_platform_date <- group_by(sets, site.platform, date)
pin_summary <- dplyr::summarize(group_by(sets, site.platform, date), mean = mean(pin_height, na.rm=TRUE), stddev = sd(pin_height, na.rm=TRUE), stderr = stddev/sqrt(length(pin_height))) %>%
  mutate(., site = substr(site.platform, 1, 4), 
         sdymin = mean - stddev, sdymax = mean + stddev,
         seymin = mean - stderr, seymax = mean + stderr)

# need to split upper and lower juncus
# JURO1-3 is lower
# 4-6 is upper
upper <- c("JURO-4", "JURO-5", "JURO-6")
lower <- c("JURO-1", "JURO-2", "JURO-3")
pin_summary$site[pin_summary$site.platform %in% upper] <- "UPJU"
pin_summary$site[pin_summary$site.platform %in% lower] <- "LOJU"

# cumulative change should be calculated here, before ymin and ymax
# cchange = mean for a date - mean from first date at that platform
# this feels like a tapply problem:
# tapply(X=mean, INDEX=site.platform)

save(sets, sets_platform_date, pin_summary, file="sets.group.R")
load("sets.group.R")

split_summary <- split(pin_summary, pin_summary$site)

ggplot(pin_summary) +
  geom_point(aes(x=date, y=mean, col=site.platform)) +
  theme_minimal()

# plot within-site variation ----
pdf(file="pin_heights_by_site_stddev_errorbars.pdf", height=8, width=11)

ggplot(split_summary$CLMA) +
  geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
  geom_errorbar(aes(x=date, ymin=sdymin, ymax=sdymax, col=site.platform)) +
  labs(x="Date", y="pin height (mean +/- std dev)", title = "within-site variation: CLMAJ") +
  theme_minimal()

ggplot(split_summary$PANN) +
  geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
  geom_errorbar(aes(x=date, ymin=sdymin, ymax=sdymax, col=site.platform)) +
  labs(x="Date", y="pin height (mean +/- std dev)", title = "within-site variation: PANNE") +
  theme_minimal()

ggplot(split_summary$UPJU) +
  geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
  geom_errorbar(aes(x=date, ymin=sdymin, ymax=sdymax, col=site.platform)) +
  labs(x="Date", y="pin height (mean +/- std dev)", title = "within-site variation: Upper Juncus") +
  theme_minimal()

ggplot(split_summary$LOJU) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=sdymin, ymax=sdymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std dev)", title = "within-site variation: Lower Juncus") +
    theme_minimal()

ggplot(split_summary$SPAL) +
  geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
  geom_errorbar(aes(x=date, ymin=sdymin, ymax=sdymax, col=site.platform)) +
  labs(x="Date", y="pin height (mean +/- std dev)", title = "within-site variation: SPAL") +
  theme_minimal()

dev.off()



# plot within-site variation ----
pdf(file="pin_heights_by_site_stderr_errorbars.pdf", height=6, width=10)

ggplot(split_summary$CLMA) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=seymin, ymax=seymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std error)", title = "within-site variation: CLMAJ") +
    theme_minimal()

ggplot(split_summary$PANN) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=seymin, ymax=seymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std error)", title = "within-site variation: PANNE") +
    theme_minimal()

ggplot(split_summary$UPJU) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=seymin, ymax=seymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std error)", title = "within-site variation: Upper Juncus") +
    theme_minimal()

ggplot(split_summary$LOJU) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=seymin, ymax=seymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std error)", title = "within-site variation: Lower Juncus") +
    theme_minimal()

ggplot(split_summary$SPAL) +
    geom_point(aes(x=date, y=mean, col=site.platform), size=2) +
    geom_errorbar(aes(x=date, ymin=seymin, ymax=seymax, col=site.platform)) +
    labs(x="Date", y="pin height (mean +/- std error)", title = "within-site variation: SPAL") +
    theme_minimal()

dev.off()


# plot within-site variation/distribution of measurements ----
pdf(file="pin_heights_by_site_boxplots.pdf", height=5, width=10)
ggplot(sets[sets$site == "CLMAJ",]) +
    geom_boxplot(aes(x=date, y=pin_height, group=interaction(site.platform, date), col=site.platform), outlier.size=2, outlier.shape=8, na.rm=TRUE) +
    labs(x="Date", y="pin height (mm)", title = "measurement distribution: CLMAJ") +
    theme_minimal()

ggplot(sets[sets$site == "PANNE",]) +
    geom_boxplot(aes(x=date, y=pin_height, group=interaction(site.platform, date), col=site.platform), outlier.size=2, outlier.shape=8, na.rm=TRUE) +
    labs(x="Date", y="pin height (mm)", title = "measurement distribution: PANNE") +
    theme_minimal()

ggplot(sets[sets$site == "UPJUN",]) +
    geom_boxplot(aes(x=date, y=pin_height, group=interaction(site.platform, date), col=site.platform), outlier.size=2, outlier.shape=8, na.rm=TRUE) +
    labs(x="Date", y="pin height (mm)", title = "measurement distribution: Upper Juncus") +
    theme_minimal()

ggplot(sets[sets$site == "LOJUN",]) +
    geom_boxplot(aes(x=date, y=pin_height, group=interaction(site.platform, date), col=site.platform), outlier.size=2, outlier.shape=8, na.rm=TRUE) +
    labs(x="Date", y="pin height (mm)", title = "measurement distribution: Lower Juncus") +
    theme_minimal()

ggplot(sets[sets$site == "SPAL",]) +
    geom_boxplot(aes(x=date, y=pin_height, group=interaction(site.platform, date), col=site.platform), outlier.size=2, outlier.shape=8, na.rm=TRUE) +
    labs(x="Date", y="pin height (mm)", title = "measurement distribution: SPAL") +
    theme_minimal()

dev.off()

