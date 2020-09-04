# Correspondence Analysis
library(readxl)
Life_Index_Inquiry_by_Company_07_2020 <- read_excel("D:/Projects/EHR/Life_Index_Inquiry_by_Company_07_2020.xlsx", 
                                                    sheet = "Life_Index_Inquiry_by_Company", 
                                                    col_types = c("numeric", "text", "date", 
                                                                  "numeric", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "numeric"))
#View(Life_Index_Inquiry_by_Company_07_2020)

life<-Life_Index_Inquiry_by_Company_07_2020[which(Life_Index_Inquiry_by_Company_07_2020$`Country Code`=="USA"),]

inq<-data.frame(company=life$`Company Name`,
                   state=life$`State Code`,
                   Cnt=life$`Inquiry Count`)

library(tidyverse)

mydata<-inq%>%
  group_by(company, state) %>%
  summarise(cnt=sum(Cnt))

mytable<- xtabs(cnt ~ company+state, data=mydata[which(mydata$company %in% 
                                                         c("Transamerica",
                                                           "Northwestern Mutual Life Ins. Co.",
                                                           "American Income Life Insurance Company",
                                                           "NML",
                                                           "Pacific Life Insurance Company",
                                                           "Country Life Ins. Co.",
                                                           "Standard Insurance Company",
                                                           "Protective Life Ins. Co.",
                                                           "Principal National Life Ins. Co.",
                                                           "Farm Bureau Life Ins. Co. of Michigan",
                                                           "Security Mutual Life Ins. Co. of New York",
                                                           "Globe Life & Accident Ins. Co.",
                                                           "Guardian Life Ins. Co. of America",
                                                           "Savings Bank Mutual Life Ins. Co. of Massachusetts (The)",
                                                           "Liberty National Life Ins. Co.",
                                                           "Brighthouse Life Insurance Company",
                                                           "National Income Life Ins. Co.",
                                                           "Lincoln National Life Ins. Co.",
                                                           "American General Life Ins. Co.",
                                                           "MIB",
                                                           "Standard Life Ins. Co. of New York",
                                                           "Catholic Order of Foresters",
                                                           "Principal Life Ins. Co.",
                                                           "Ohio National Life Insurance Company",
                                                           "Ohio National Life Assurance Corporation"
                                                         )), ])

library(ca)
#mytable <- with(mydata, table(company, state)) # create a 2 way table
#prop.table(mytable, 1) # row percentages
#prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
print(fit) # basic results
summary(fit) # extended results
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

cmp_coor<-fit$rowcoord[, 1:2]
st_coor<-fit$colcoord[, 1:2]

library(xlsx)
write.xlsx(cmp_coor, "D:/Projects/EHR/cmp_coor.xlsx")
write.xlsx(st_coor, "D:/Projects/EHR/st_coor.xlsx")