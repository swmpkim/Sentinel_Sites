# this should be used in conjunction with SETs.Rmd


DI <- read.csv("DI_water_levels.csv")
head(DI)
DI$date <- paste0(DI$Year, "-", DI$Month, "-01")
DI$date <- ymd(DI$date)
library(lubridate)
DI$date <- ymd(DI$date)

DI2 <- DI %>% subset(date >= "2012-02-01")
DI2 <- DI2 %>% subset(date <= "2016-11-01")
DI3 <- DI2 %>%
select(-Year, -Month, -HWI, -LWI, -Inferred, -DHQ, -DLQ, -GT, -MN) %>%
gather(key = "datum", value = "level", -date)
ggplot(DI3, aes(x=date, y=level)) +
geom_line(aes(col=datum)) +
geom_smooth(method="lm", aes(col=datum), se=FALSE, lty=2, lwd=1, alpha=0.6) +
theme_minimal()





ggplot() +
    geom_line(data = DI, aes(x = date, y = MSL), lty=2, alpha=0.5) +
    geom_smooth(method="lm", data=DI, aes(x=date, y=MSL), se=FALSE) +
    geom_line(data = DI, aes(x = date, y = MHW), lty=2, alpha=0.5) +
    geom_smooth(method="lm", data=DI, aes(x=date, y=MHW), se=FALSE) +
    geom_point(data = pin_change, aes(x=date, y=meanadj, col = site), alpha=0.7, size=3) +
    theme_bw()

ggplot(pin_change) +
    geom_point(aes(x=date, y=meanadj, col=site.platform)) +
    geom_point(data = DI, aes(x = date, y = MSL), alpha = 0.3) + 


ggplot(pin_change, aes(x=date, y=meanadj)) +
    geom_point(aes(col=site.platform), alpha=0.5, size=3)

DI3 <- DI2 %>%
    select(-Year, -Month, -HWI, -LWI, -Inferred, -DHQ, -DLQ, -GT, -MN) %>%
    gather(key = "datum", value = "level", -date)

ggplot(DI3, aes(x=date, y=level)) +
    geom_line(aes(col=datum)) +
    theme_minimal() 

ggplot(DI3, aes(x=date, y=level)) +
    geom_line(aes(col=datum)) + 
    geom_smooth(method="lm", aes(col=datum), se=FALSE, lty=2, lwd=1, alpha=0.6) +
    theme_minimal() 
