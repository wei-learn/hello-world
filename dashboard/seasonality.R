#Decompose a time series

library(readxl)
Life_Index_Legacy_Vs_New_0827 <- read_excel("D:/Projects/Life_index/Life Index Legacy Vs New_0827.xlsx", 
                                            sheet = "worksheet", col_types = c("numeric", 
                                                                               "text", "date", "numeric", "numeric"))
#View(Life_Index_Legacy_Vs_New_0827)
library(xts)
li <- xts(Life_Index_Legacy_Vs_New_0827[, 4:5], as.Date(Life_Index_Legacy_Vs_New_0827$Date))

#define seasonality
li_new <- ts(li$New, frequency = 12 )
decomp <- decompose (li_new, type = "multiplicative")
plot(decomp)
plot(decomp$figure, xlab="Month", ylab = "Monthly Factor", main = "Life Index Seasonality",
     col = "red", type="p")
abline(h=1, lty=2, col="gray")
text(decomp$figure,labels=format(decomp$figure, digits=2, nsmall=2))

# 1.0190120 1.0470165 1.0791557 1.0365262 0.9717072 0.9456064 0.8970007 0.9823094 0.9008516
# 1.0646482 1.0896192 0.9665471

library(xlsx)
write.xlsx(decomp$figure, "D:/Projects/EHR/Life_Ind_Seasonality.xlsx")

#define seasonality
li_old <- ts(li$Legacy, frequency = 12 )
decomp <- decompose (li_old, type = "multiplicative")
plot(decomp)
plot(decomp$figure, xlab="Month", ylab = "Monthly Factor", main = "Legacy Life Index Seasonality",
     col = "red", type="p")
abline(h=1, lty=2, col="orange")
text(decomp$figure,labels=format(decomp$figure, digits=2, nsmall=2))

# 1.0301857 1.0561339 1.0938204 1.0158596 0.9462226 0.9579726 0.9137964 0.9230839 0.9596809
# 0.9995150 1.0872993 1.0164296