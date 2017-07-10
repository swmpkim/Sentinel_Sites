library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites")

# format feldspar application data
feldspar <- read.csv("feldspar_apps.csv")
# glue site.platform and plat together
feldspar <- mutate(feldspar, site.platform.plat = paste0(site.platform, "_", plat))
# put application dates in date format
feldspar$feldspar_1 <- mdy(feldspar$feldspar_1)
feldspar$feldspar_2 <- mdy(feldspar$feldspar_2)


# read marker horizon data; glue site, platform, plat, and rep together into one identifier; gather into a long dataset
markers <- read.csv("marker_horizons.csv")
markers <- markers %>%
    mutate(site.platform.plat.rep = paste0(site.platform, "_", plat, "_", rep)) %>%
    gather(key = date, value = sed_height, -site.platform.plat.rep, -site.platform, -plat, -rep)

# remove the 'X' before the date and put it in date format
markers$date <- sub('.', '', markers$date) 
markers$date <- mdy(markers$date)

# give it sites:
markers <- mutate(markers, site = substr(site.platform, 1, 4))

# need to split upper and lower juncus
# JURO1-3 is lower
# 4-6 is upper
upper <- c("JURO_4", "JURO_5", "JURO_6")
lower <- c("JURO_1", "JURO_2", "JURO_3")
markers$site[markers$site.platform %in% upper] <- "UPJU"
markers$site[markers$site.platform %in% lower] <- "LOJU"


png("marker_horizons.png", height=11, width=8, units = "in", res = 400)
ggplot(markers) +
    geom_point(aes(x=date, y=sed_height, col=as.factor(plat)), size=2, alpha=0.7) +
    facet_wrap(~site.platform, ncol=3) +
    theme_bw()
dev.off()


# make NAs something that will show up on the graph. -10?
markers$sed_height_noNA <- markers$sed_height
markers$sed_height_noNA[is.na(markers$sed_height_noNA)] <- -10

png("marker_horizons_withNAs.png", height=11, width=8, units = "in", res = 400)
ggplot(markers) +
    geom_point(aes(x=date, y=sed_height_noNA, col=as.factor(plat)), size=2, alpha=0.7) +
    facet_wrap(~site.platform, ncol=3) +
    theme_bw()
dev.off()


# make graphs by site.platform
pdf("marker_horizons_by_SETplatform_withNAs.pdf", height=11, width=8)
for(i in levels(markers$site.platform)) {
    dat <- markers[markers$site.platform == i,]
    feldspardates <- feldspar[feldspar$site.platform == i, "feldspar_2"]
    g <- ggplot(dat) +
        geom_point(aes(x=date, y=sed_height_noNA, col=as.factor(rep)), size=3, alpha=0.7) +
        geom_vline(feldspardates)
        facet_wrap(~site.platform + plat, ncol=1) +
        theme_bw()
    print(g)
}
dev.off()



# group by site.platform.plat and summarize:
plats <- markers %>%
    mutate(site.platform.plat = paste0(site.platform, "_", plat)) %>%
    group_by(site.platform.plat, date) %>%
    dplyr::summarize(sed_mean = mean(sed_height, na.rm=TRUE), sed_stdev = sd(sed_height, na.rm=TRUE), sed_stderr = sed_stdev/sqrt(length(sed_height)))

# give it sites:
plats <- mutate(plats, 
                site = substr(site.platform.plat, 1, 4), 
                site.platform=substr(site.platform.plat, 1, nchar(site.platform.plat)-2), 
                plat=substr(site.platform.plat, nchar(site.platform.plat), nchar(site.platform.plat)))

# need to split upper and lower juncus
# JURO1-3 is lower
# 4-6 is upper
upper <- c("JURO_4", "JURO_5", "JURO_6")
lower <- c("JURO_1", "JURO_2", "JURO_3")
plats$site[plats$site.platform %in% upper] <- "UPJU"
plats$site[plats$site.platform %in% lower] <- "LOJU"
plats$site <- factor(plats$site)

# plot that
pdf("marker_horizon_means_by_SETplatform.pdf", height=11, width=8)

for(i in levels(plats$site)) {
    dat <- plats[plats$site == i,]

    g <- ggplot(dat) +
        geom_point(aes(x=date, y=sed_mean, col=as.factor(plat)), size=2, alpha=0.7) +
        geom_errorbar(aes(x = date, 
                          ymin=sed_mean - sed_stderr, 
                          ymax=sed_mean + sed_stderr, 
                          col=as.factor(plat))) +
        facet_wrap(~site.platform, ncol=1) +
        theme_bw()
    print(g)
}
dev.off()
