# Get SET data into proper wide format: one column for arm position; 9 columns for pins (rather than 36 columns that go arm-pin)

setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/Sentinel_Sites/f-backup spreadsheets/sets_formatting")

dat <- read.csv("sets_wide_CORRECTED.csv", stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(tidyr)

dat2 <- dat %>%
    gather(key="arm.pin", value="value", -SET, -date) %>%
    mutate(arm = substr(arm.pin, 2, 3), pin = paste0("pin_", substr(arm.pin, 5, 5))) %>%
    select(SET, date, arm, pin, value) %>%
    spread(key=pin, value=value)

write.csv(dat2, "sets_wide_UPDATED.csv", row.names = FALSE)


dat3 <- dat %>%
    gather(key="arm.pin", value="value", -SET, -date) %>%
    mutate(arm = substr(arm.pin, 2, 3), pin = paste0("pin_", substr(arm.pin, 5, 5))) %>%
    select(SET, date, arm, pin, value)

write.csv(dat3, "sets_long.csv", row.names=FALSE)
