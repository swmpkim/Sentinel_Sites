# library(tidyverse)


# here is the function attempt.

attach_site_names <- function(dat, set_clm = "SET") {
    
    # set_clm defaults to being named "SET" - needs to be specified, in quotes, if it's something different
    
    siteID <- data.frame("CLMA" = c("CLMAJ-1", "CLMAJ-2", "CLMAJ-3"), 
                           "PANN" = c("PANNE-1", "PANNE-2", "PANNE-3"),
                           "JULO" = c("JURO-1", "JURO-2", "JURO-3"),
                           "JUUP" = c("JURO-4", "JURO-5", "JURO-6"),
                           "SPAL" = c("SPAL-1", "SPAL-2", "SPAL-3")) %>%
        gather(key = "site", value = "SET") %>%
        mutate(site = as.factor(site),
               SET = as.factor(SET))
    
    # make sure the original SET names column is named "SET" in the output; this also makes left_join possible
    dat <- rename(dat, SET = !! set_clm)
    
    # join that with the original data frame
    dat_out <- left_join(dat, siteID, by = "SET")
    
    return(dat_out)
}

