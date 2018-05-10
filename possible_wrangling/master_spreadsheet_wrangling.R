# attempts to use tidyr::fill to deal with some of the spreadsheet craziness

library(tidyverse)
library(readxl)
library(lubridate)

dat <- read_excel("bane_of_existence.xlsx")

dat2 <- dat %>%
    rename(pin = X__1) %>%  # rename the first column
    separate(pin, into = c("site", "platform", "arm", "pin")) %>%  # break up identifier row into 4 sub-pieces of info 
    mutate(pin = case_when(is.na(pin) == "FALSE" ~ pin,
                           is.na(pin) == "TRUE" ~ site),  # keep pin where it exists; where it does not, pull over the value from site
           site = case_when(site != pin ~ site)) %>%   # keep site where it is distinct from pin; when this condition is not met it becomes NA
    fill(site, platform, arm) # fill down these columns with what was above it

head(dat2)


# deal with the column headings being dates
dat3 <- dat2 %>%
    gather(key = "date", value = "value", -site, -platform, -arm, -pin)

# don't know how to deal with this because 8/23/2011 got converted to 40778
# 40778 + 1900-01-01 = 8/25/2011 according to lubridate's math
# argh
    


