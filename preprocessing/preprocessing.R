load("~/waterwaiter/Water_WHW.RData")
x <- x %>% mutate(t=as_datetime(unix_ts)%>%with_tz("America/New_York")) %>% select(-unix_ts,-counter,-inst_rate) %>%
  filter(year(t)==2012)
save(x,file="~/waterwaiter/water.RData")
