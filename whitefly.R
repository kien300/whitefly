library(tidyverse) #general data 
library(readxl)

setwd("D:/OneDrive - CGIAR/CIAT/OTA/Nami'sData_2.9.18")

cm <- read_excel("Whitefly_data entry_template_Cambodia.xls")
vn <- read_excel("Whitefly_data entry_template_Vietnam.xls")

#general data cleaning------
table(cm$Transect_A_adult)
sum(is.na(cm$Transect_A_adult))

cm <- cm %>% filter(!is.na(cm$Transect_A_adult)) #879-79=800

cm <- cm %>% do(na.locf(.)) #replace NA with the most recent non-NA

cm$A_Nymph_Class <- NA
table(cm$Transect_A_nymph) # two 0s --> wrong
cm$Transect_A_nymph[cm$Transect_A_nymph==0] <- 1

cm$A_Nymph_Class[cm$Transect_A_nymph==1] <- "0 to 5"
cm$A_Nymph_Class[cm$Transect_A_nymph==2] <- "5 to 10"
cm$A_Nymph_Class[cm$Transect_A_nymph==3] <- "10 to 25"
cm$A_Nymph_Class[cm$Transect_A_nymph==4] <- "25 to 50"
cm$A_Nymph_Class[cm$Transect_A_nymph==5] <- "50 and more"
table(cm$A_Nymph_Class)

cm$B_Nymph_Class <- NA
table(cm$Transect_B_nymph)

cm$B_Nymph_Class[cm$Transect_B_nymph==1] <- "0 to 5"
cm$B_Nymph_Class[cm$Transect_B_nymph==2] <- "5 to 10"
cm$B_Nymph_Class[cm$Transect_B_nymph==3] <- "10 to 25"
cm$B_Nymph_Class[cm$Transect_B_nymph==4] <- "25 to 50"
cm$B_Nymph_Class[cm$Transect_B_nymph==5] <- "50 and more"
table(cm$B_Nymph_Class)

cm$Transect_A_adult <- as.numeric(cm$Transect_A_adult)
cm$Transect_B_adult <- as.numeric(cm$Transect_B_adult)

#calculation-----
sum_CM <- cm %>% group_by(Collector, Country, Province, District, GPS_Latitude, GPS_Longitude) %>% 
  summarise(SUM_A_adult = sum(Transect_A_adult),
            SUM_B_adult = sum(Transect_B_adult))

table(sum_CM$Collector, sum_CM$SUM_A_adult)
table(sum_CM$Province, sum_CM$SUM_A_adult)
table(sum_CM$District, sum_CM$SUM_A_adult)
