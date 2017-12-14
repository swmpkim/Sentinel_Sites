# test to see if outputs are different when calculating individual slopes for pins vs. averaging pins across SETs

library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


# setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel Sites")

# load group of files, generated from SET_summarizing.R
load("sets.group.R")

# need to work with individual pins, so that's sets_platform_date
# paste together a couple columns to get identifiers for position and pin,
# split upper and lower juncus,
# and clean up
indv_pins <- sets_platform_date %>%
    ungroup() %>%
    mutate(ind_pin = paste0(site.platform.position, "-", pin_number), 
           site = substr(site.platform.position, 1, 4)) %>%
    select(ind_pin, date, pin_height, site.platform.position, site, site.platform)
upper <- c("JURO-4", "JURO-5", "JURO-6")
lower <- c("JURO-1", "JURO-2", "JURO-3")
indv_pins$site[indv_pins$site.platform %in% upper] <- "UPJU"
indv_pins$site[indv_pins$site.platform %in% lower] <- "LOJU"
indv_pins <- indv_pins %>%
    select(-site.platform)

# to get change for each pin, need to split apply combine on the ind_pin level
pin_split <- split(indv_pins, indv_pins$ind_pin) %>%
    lapply(., FUN = function(x) {mutate(x, "change" = pin_height - pin_height[1])})
pin_change <- do.call(rbind, pin_split) 


# can run models on them individually...
# make a loop to just do CLMAJ
CLMAJ1 <- data.frame(36, 2)
for(i in 1:36) {
test <- data.frame(pin_split[i])
names(test) <- substr(names(test), 14, nchar(names(test)))
summary(test$change)
testlm <- lm(change ~ date, data=test)
summary(testlm)
CLMAJ1[i,] <- coef(testlm)
}

summary(CLMAJ1$X2)
round((mean(CLMAJ1$X2) * 4 * 365 + 1)/4, 3) # = 3.874 mm/yr
# CLMAJ1 from averaged first was 3.861 


# now do CLMAJ2
CLMAJ2 <- data.frame(36, 2)
for(i in 37:72) {
    test <- data.frame(pin_split[i])
    names(test) <- substr(names(test), 14, nchar(names(test)))
    summary(test$change)
    if(anyNA(test$change) == TRUE) next
    testlm <- lm(change ~ date, data=test)
    summary(testlm)
    CLMAJ2[i-36,] <- coef(testlm)
}

summary(CLMAJ2$X2)
round((mean(CLMAJ2$X2, na.rm=TRUE) * 4 * 365 + 1)/4, 3) # 5.123
# other way of doing it: 5.123


CLMAJ3 <- data.frame(36, 2)
for(i in 73:108) {
    test <- data.frame(pin_split[i])
    names(test) <- substr(names(test), 14, nchar(names(test)))
    summary(test$change)
    testlm <- lm(change ~ date, data=test)
    summary(testlm)
    CLMAJ3[i-72,] <- coef(testlm)
}

summary(CLMAJ3$X2)
round((mean(CLMAJ3$X2) * 4 * 365 + 1)/4, 3) # = 4.539 mm/yr
# CLMAJ3 from averaged first was 4.539



# now do JURO-1
JURO1 <- data.frame(36, 2)
ns <- grep("JURO-1", names(pin_split))
for(i in ns) {
    test <- data.frame(pin_split[i])
    names(test) <- substr(names(test), 13, nchar(names(test)))
    summary(test$change)
    if(anyNA(test$change) == TRUE) next
    testlm <- lm(change ~ date, data=test)
    summary(testlm)
    JURO1[i-ns[1]+1,] <- coef(testlm)
}

summary(JURO1$X2)  # 3 NAs
round((mean(JURO1$X2, na.rm=TRUE) * 4 * 365 + 1)/4, 3) # 6.684
# other way of doing it: 6.499

### slightly more automated ----
### just change the grep and the number of chracters to strip from the name
### and do all the sites
# now do JURO-2
rm(PANNE)
PANNE <- data.frame(36, 2)
ns <- grep("CLMAJ-3", names(pin_split))
for(i in ns) {
    test <- data.frame(pin_split[i])
    names(test) <- substr(names(test), 14, nchar(names(test)))
    summary(test$change)
    if(anyNA(test$change) == TRUE) next
    testlm <- lm(change ~ date, data=test)
    summary(testlm)
    PANNE[i-ns[1]+1,] <- coef(testlm)
}

summary(PANNE$X2)
round((mean(PANNE$X2, na.rm=TRUE) * 4 * 365 + 1)/4, 3) 


# t-test on differences:
diffs <- c(-0.073, 0, 0, 0.185, 0.038, -0.048, 0, 0, 0, 0, -0.005, -0.021, -0.169, 0.248, 0.084)
t.test(diffs, arg = "two.sided")

# differences when na is >0:
diffs <- c(-0.073, 0.185, 0.038, -0.048, -0.005, -0.021, -0.169, 0.248)
t.test(diffs, arg = "two.sided")

# linear regressions on each pin
models <- pin_change %>%
    group_by(ind_pin) %>%
    do(mod = lm(change ~ date, data=., na.action=na.exclude))
modelcoef_indpins <- tidy(models, mod, conf.int=TRUE, conf.level=0.95) # this gives the intercept, slope, and p values, among others; conf.int=TRUE, conf.level=0.95 gives the 95% confidence interval
# make a slope per year for date estimate; NAs for intercept
modelcoef_indpins$mm.yr <- ifelse(modelcoef_indpins$term == "date", round((modelcoef_indpins$estimate * 4 * 365 + 1)/4, 3), "NA")
# confidence intervals in mm/yr:
modelcoef_indpins$conf.low.yr <- ifelse(modelcoef_indpins$term == "date", round((modelcoef_indpins$conf.low * 4 * 365 + 1)/4, 3), "NA")
modelcoef_indpins$conf.high.yr <- ifelse(modelcoef_indpins$term == "date", round((modelcoef_indpins$conf.high * 4 * 365 + 1)/4, 3), "NA")
modelsummary_indpins <- glance(models, mod) # this gives r^2 and adjusted r^2, and p values, among others

write.csv(modelsummary_indpins, "modelsummary_by_indpins.csv", row.names=FALSE)
write.csv(modelcoef_indpins, "modelcoef_by_indpins.csv", row.names=FALSE)

