library(highcharter)
library(tidyverse)
library(xts)

dt<- seq(as.Date("2017-01-01"), as.Date("2017-01-05"), "days")
n<-length(dt)
x <- xts(rnorm(n), order.by=dt)
y <- xts(rnorm(n)+1, order.by=dt)

highchart(type = "stock") %>% 
  hc_add_annotation(
    labelOptions = list(
      backgroundColor = 'rgba(255,255,255,0.5)',
      verticalAlign = 'top',
      y = 15
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = datetime_to_timestamp(as.Date("2017/01/02")),
          y = 1.5
        ),
        text = "Some annotation",
	  shape = "connector"
      )
    ),
    shapes = list(
         list(
           point = list(
             xAxis = 0,
             yAxis = 0,
             x = datetime_to_timestamp(as.Date("2017/01/03")),
             y = 1
        ),
           type = "circle",
           r = 10,
           fill = "red"
        )
     )
      
  ) %>% 
  hc_xAxis(
    minRange = 1
  ) %>% 
 hc_add_series(x, color='green', name="x", showInLegend = TRUE) %>%
 hc_add_series(y, color='red', name="y", showInLegend = TRUE)
  