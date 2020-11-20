source("./inst/dependencies.R")
library(devtools)
devtools::load_all()
library(propalloc)
library(dplyr)

data<- propalloc::loss_run

newdata <- data %>%
  group_by(year) %>%
  summarise(inc = sum(total_incurred,na.rm = TRUE),
            cnt = n()) %>%
  ungroup()%>%
  mutate(severity=inc/cnt)

highchart() %>%
  highcharter::hc_chart(
  type = "column") %>%
hc_add_series(data = newdata$severity)
