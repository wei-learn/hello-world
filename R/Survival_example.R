#install.packages(c("survival", "lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
#remotes::install_github("zabore/condsurv")
#remotes::install_github("zabore/ezfun")
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
#library(condsurv)
library(dplyr)
library(dtplyr)
# change the censor indicator
lung <- 
  lung %>% 
  mutate(
#    status = recode(status, `1` = 0, `2` = 1)
    status = case_match(status, "1"~0, "2"~1, .default = species)
  )

#Surv(lung$time, lung$status)[1:10]

#creating Kaplan-Meier estimates
s1 <- survfit(Surv(time, status) ~ 1, data = lung)
str(s1)

survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit(linewidth=1, color='grey50') +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval(fill='blue') +
  add_risktable() +
  add_quantile(y_value = 0.6)

#by sex
survfit2(Surv(time, status) ~ sex, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable() 

#+
#  add_quantile(y_value = 0.5, color = "forestgreen", linewidth = 0.75)

