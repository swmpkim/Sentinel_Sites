# imported and formatted data through SETs.Rmd and SETs plus MHs.Rmd
# need to correct dates
# to do that, I want to get these data frames into wide format

# sets
sets.wide <- sets %>%
    mutate(arm_pin = paste0(arm_position, "-", pin_number)) %>%
    select(site.platform, date, arm_pin, pin_height) %>%
    spread(key = arm_pin, value = pin_height) %>%
    arrange(date)
write.csv(sets.wide, "sets_wide.csv", row.names = FALSE)


# marker horizons
markers.wide <- markers %>%
    mutate(plat_rep = paste0(plat, "-", rep)) %>%
    select(site.platform, date, plat_rep, sed_height) %>%
    spread(key = plat_rep, value = sed_height) %>%
    arrange(date)
write.csv(markers.wide, "markers_wide.csv", row.names=FALSE)
