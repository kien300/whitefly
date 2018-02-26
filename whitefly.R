library(tidyverse) #general data 
library(readxl) #import Excel files
library(zoo) #fill in data
library(vcd) #Association Plort
library(fifer) #post-hoc Chi Squared test 
library(knitr) #for table format

setwd("D:/OneDrive - CGIAR/CIAT/OTA/Nami'sData_2.9.18")

cm <- read_excel("for analysis/Cambodia.xls")
vn <- read_excel("for analysis/Vietnam.xls")

#Cambodia----------------------
#general data cleaning------
#table(cm$Transect_A_adult)
#sum(is.na(cm$Transect_A_adult))

cm <- cm %>% filter(!is.na(cm$Transect_A_adult)) #879-79=800

cm <- cm %>% do(na.locf(.)) #replace NA with the most recent non-NA

cm$A_Nymph_Class <- NA
#table(cm$Transect_A_nymph) # two 0s --> wrong
cm$Transect_A_nymph[cm$Transect_A_nymph==0] <- 1

cm$A_Nymph_Class[cm$Transect_A_nymph==1] <- "0 to 5"
cm$A_Nymph_Class[cm$Transect_A_nymph==2] <- "5 to 10"
cm$A_Nymph_Class[cm$Transect_A_nymph==3] <- "10 to 25"
cm$A_Nymph_Class[cm$Transect_A_nymph==4] <- "25 to 50"
cm$A_Nymph_Class[cm$Transect_A_nymph==5] <- "50 and more"
#table(cm$A_Nymph_Class)

cm$B_Nymph_Class <- NA
#table(cm$Transect_B_nymph)

cm$B_Nymph_Class[cm$Transect_B_nymph==1] <- "0 to 5"
cm$B_Nymph_Class[cm$Transect_B_nymph==2] <- "5 to 10"
cm$B_Nymph_Class[cm$Transect_B_nymph==3] <- "10 to 25"
cm$B_Nymph_Class[cm$Transect_B_nymph==4] <- "25 to 50"
cm$B_Nymph_Class[cm$Transect_B_nymph==5] <- "50 and more"
#table(cm$B_Nymph_Class)

cm$Transect_A_adult <- as.numeric(cm$Transect_A_adult)
cm$Transect_B_adult <- as.numeric(cm$Transect_B_adult)

#combine A & B Transect
cm_combined <- cm %>% 
  dplyr::select(District, `Field ID`, Transect_A_adult, Transect_B_adult) %>% 
  gather(Transect_A_adult, Transect_B_adult, key = 'Transect', value = 'Count')

#test-----
# sum_CM <- cm %>% group_by(Collector, Country, Province, District, `Field ID`) %>% 
#   summarise(SUM_A_adult = sum(Transect_A_adult),
#             SUM_B_adult = sum(Transect_B_adult))

sum_CM <- cm_combined %>% group_by(District, `Field ID`) %>% 
    summarise(SUM_adult = sum(Count))

# table(sum_CM$Collector, sum_CM$SUM_A_adult)
# table(sum_CM$Province, sum_CM$SUM_A_adult)
# table(sum_CM$District, sum_CM$SUM_A_adult)

#ANOVA for different Districts-----
#Transect A
#boxplot(SUM_A_adult~District, data = sum_CM, horizontal=T) #to make visible
#results1 = aov(SUM_A_adult~District, data = sum_CM)
results1 = aov(SUM_adult~District, data = sum_CM)
#summary(results1) #to make visible
#TukeyHSD(results1) #post-hoc test for pairwise comparision, perform if anova was significant
#p-value=.408, fails to reject the null <=> No difference in Districts mean

#Transect B
#boxplot(SUM_B_adult~District, data = sum_CM, horizontal=T) #to make visible
#results2 = aov(SUM_B_adult~District, data = sum_CM)
#summary(results2) #to make visible
#p-value=.166, fails to reject the null <=> No difference in Districts mean

#test for nymph----
##table(cm$Transect_A_nymph) #make visile for Markdown files
##table(cm$Transect_B_nymph) #make visile for Markdown files

#Vietnam--------------------
#general tidying---------
#table(vn$Transect_A_adult)

vn$Transect_A_adult[vn$Transect_A_adult=='-'] <- 'NA'
#sum(is.na(vn$Transect_A_adult)) #check

vn <- vn %>% filter(!is.na(vn$Transect_A_adult)) #846-116=730

#table(vn$Transect_B_adult)
vn$Transect_B_adult[vn$Transect_B_adult=='-'] <- 'NA'
#sum(is.na(vn$Transect_B_adult)) #check

vn <- vn %>% do(na.locf(.)) #replace NA with the most recent non-NA

vn$Transect_A_adult <- as.numeric(vn$Transect_A_adult)
vn$Transect_B_adult <- as.numeric(vn$Transect_B_adult)

#table(vn$District) #see sample count per District
#eliminate districts with fewer than 5 samples
vn <- vn %>% filter(District != "Thuan Chau" & District != "Van Yen")

#combine A & B Transect
vn_combined <- vn %>% 
  dplyr::select(District, `Field ID`, Transect_A_adult, Transect_B_adult) %>% 
  gather(Transect_A_adult, Transect_B_adult, key = 'Transect', value = 'Count')

#test-----
# sum_VN <- vn %>% group_by(Collector, Country, Province, District, `Field ID`) %>% 
#   summarise(SUM_A_adult = sum(Transect_A_adult, na.rm = TRUE),
#             SUM_B_adult = sum(Transect_B_adult, na.rm = TRUE))
sum_VN <- vn_combined %>% group_by(District, `Field ID`) %>% 
  summarise(SUM_adult = sum(Count, na.rm = TRUE))

#ANOVA for different Districts-----
#Transect A
#boxplot(SUM_A_adult~District, data = sum_VN, horizontal=T) #to make visible
results3 = aov(SUM_adult~District, data = sum_VN)
#summary(results3) #to make visible
#post-hoc test for significant ANOVA #to make visible
tukey3 <- as.data.frame(TukeyHSD(results3)$District)
tukey3 <- rownames_to_column(tukey3, "Pair") %>% 
  filter(`p adj`<0.05)
#p-value is significant, reject the null(No difference in Districts mean)

#Transect B
#boxplot(SUM_B_adult~District, data = sum_VN, horizontal=T) #to make visible
#results4 = aov(SUM_B_adult~District, data = sum_VN)
#summary(results4) #to make visible
#post-hoc test for significant ANOVA #to make visible
#tukey4 <- as.data.frame(TukeyHSD(results4)$District)
# tukey4 <- rownames_to_column(tukey4, "Pair") %>% 
#   filter(`p adj`<0.05)
#p-value is significant, fails to reject the null(No difference in Districts mean)

#test for nymph-----
#table(vn$Transect_A_nymph)
vn$Transect_A_nymph[vn$Transect_A_nymph=='0'] <- 'NA'
vn$Transect_A_nymph <- as.numeric(vn$Transect_A_nymph)
#sum(is.na(vn$Transect_A_nymph)) #check

vn$A_Nymph_Class <- NA
vn$A_Nymph_Class[vn$Transect_A_nymph==1] <- "0 to 5"
vn$A_Nymph_Class[vn$Transect_A_nymph==2] <- "5 to 10"
vn$A_Nymph_Class[vn$Transect_A_nymph==3] <- "10 to 25"
vn$A_Nymph_Class[vn$Transect_A_nymph==4] <- "25 to 50"
vn$A_Nymph_Class[vn$Transect_A_nymph==5] <- "50 and more"

vn$A_Nymph_Class <- as.factor(vn$A_Nymph_Class)
#table(vn$A_Nymph_Class)
vn_AN <- vn %>% filter(!is.na(A_Nymph_Class)) %>% 
  group_by(District, A_Nymph_Class) %>% 
  summarise(Freq=n())
vn_AN$A_Nymph_Class <- factor(vn_AN$A_Nymph_Class,levels(vn_AN$A_Nymph_Class)[c(1,3,2)])


vn_AN1 <- vn_AN %>% spread(A_Nymph_Class, Freq)
vn_AN1[is.na(vn_AN1)] <- 0
vn_AN1 <- remove_rownames(vn_AN1)
vn_AN1 <- column_to_rownames(vn_AN1, var = "District")

#chisq.test(vn_AN1) #make visible in Markdown
Chisq_vnA <- 
  chisq.post.hoc(vn_AN1, test = c("fisher.test"), popsInRows = TRUE,control = c("BH"), digits = 4) %>% 
  filter(adj.p<0.05)

vn_AN2 <- structable(~District+A_Nymph_Class,data=vn_AN)
#assoc(vn_AN2, shade=TRUE) #to make visible

#table(vn$Transect_B_nymph)
vn$Transect_B_nymph[vn$Transect_B_nymph=='0'] <- 'NA'
vn$Transect_B_nymph <- as.numeric(vn$Transect_B_nymph)
#sum(is.na(vn$Transect_B_nymph)) #check

vn$B_Nymph_Class <- NA
vn$B_Nymph_Class[vn$Transect_B_nymph==1] <- "0 to 5"
vn$B_Nymph_Class[vn$Transect_B_nymph==2] <- "5 to 10"
vn$B_Nymph_Class[vn$Transect_B_nymph==3] <- "10 to 25"
vn$B_Nymph_Class[vn$Transect_B_nymph==4] <- "25 to 50"
vn$B_Nymph_Class[vn$Transect_B_nymph==5] <- "50 and more"

vn$B_Nymph_Class <- as.factor(vn$B_Nymph_Class)
#table(vn$B_Nymph_Class)
vn_BN <- vn %>% filter(!is.na(B_Nymph_Class)) %>% 
  group_by(District, B_Nymph_Class) %>% 
  summarise(Freq=n())
vn_BN$B_Nymph_Class <- factor(vn_BN$B_Nymph_Class,levels(vn_BN$B_Nymph_Class)[c(1,4,2,3)])

vn_BN1 <- vn_BN %>% spread(B_Nymph_Class, Freq)
vn_BN1[is.na(vn_BN1)] <- 0
vn_BN1 <- remove_rownames(vn_BN1)
vn_BN1 <- column_to_rownames(vn_BN1, var = "District")

#chisq.test(vn_BN1) #to make visible
Chisq_vnB <- 
  chisq.post.hoc(vn_BN1, test = c("fisher.test"), popsInRows = TRUE,control = c("BH"), digits = 4) %>% 
  filter(adj.p<0.05)

vn_BN2 <- structable(~District+B_Nymph_Class,data=vn_BN)
#assoc(vn_BN2, shade=TRUE) #to make visible