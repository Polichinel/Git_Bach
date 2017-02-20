#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++PITF model++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library("readstata13")

PITF_data <- read.dta13("/home/polichinel/Dropbox/KU/6.Semester/Bach/Litteratur_og_data/Goldstone_data_script_ect/PITF Global Model Replication Data 121709.dta")
str(PITF_data$sftptv2a)

# Konstruere dikotom variabler for hver system type
PITF_data$sftptv2a1 <- as.numeric(PITF_data$sftptv2a %in% "Full autocracy")
PITF_data$sftptv2a2 <- as.numeric(PITF_data$sftptv2a %in% "Full democracy")
PITF_data$sftptv2a3 <- as.numeric(PITF_data$sftptv2a %in% "Partial autocracy")
PITF_data$sftptv2a4 <- as.numeric(PITF_data$sftptv2a %in% "Partial democracy with factionalism")
PITF_data$sftptv2a5 <- as.numeric(PITF_data$sftptv2a %in% "Partial democracy without factionalism")
PITF_data$sftptv2a6 <- as.numeric(PITF_data$sftptv2a %in%  "Transition")

# Konstruere dikotom variable for hvar region -> noter dig at Nord Amerika (NA) ikke er med i Goldstones dataset: Det skal du tilføje.
PITF_data$byregn2_1 <- as.numeric(PITF_data$byregn2 %in% 1) # Afrika
PITF_data$byregn2_2 <- as.numeric(PITF_data$byregn2 %in% 2) # Asien
PITF_data$byregn2_3 <- as.numeric(PITF_data$byregn2 %in% 3) # Europa
PITF_data$byregn2_4 <- as.numeric(PITF_data$byregn2 %in% 4) # Latin Amerika
PITF_data$byregn2_5 <- as.numeric(PITF_data$byregn2 %in% 5) # Mellem østen

# Konstruere Stratum ID
stratid <- as.character(paste(PITF_data$year, PITF_data$byregn2, PITF_data$group, sep = ""))

PITF_data$stratida <- stratid[PITF_data$sample == 1] 
PITF_data$stratidb <- stratid[PITF_data$sample == 2]
PITF_data$stratidc <- stratid[PITF_data$sample == 3]
# meget elegant - men nu gentages værdien blot for alle obs... Se hvad der sker..


# MODELLER
# Første stata kode..
# clogit sftpcons sftptv2a3 sftptv2a4 sftptv2a5 sftptv2a2 sftptv2a6 logim maccat disp4cat if sample==1 & glb_ind=="Y", group(stratida) 
#Laver nu data frame

library(survival)
# for clogit

# Full problem set +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_f1 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratida),
                   PITF_data, subset = sample==1 & glb_ind=="Y")
summary(model_f1)  

model_f2 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidb),
                   PITF_data, subset = sample==2 & glb_ind=="Y")
summary(model_f2)  

model_f3 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidc),
                   PITF_data, subset = sample==3 & glb_ind=="Y")
summary(model_f3) 
# problemer med sftptv2a6

# Civil War Onsets ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_c1 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratida),
                   PITF_data, subset = sample==1 & cwar_ind=="Y")
summary(model_c1)  

model_c2 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidb),
                   PITF_data, subset = sample==2 & cwar_ind=="Y")
summary(model_c2)  

model_c3 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidc),
                   PITF_data, subset = sample==3 & cwar_ind=="Y")
summary(model_c3) 
# Igen problemer med sftptv2a6

# Adverse Regime Change Onsets +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_r1 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratida),
                   PITF_data, subset = sample==1 & reg_ind=="Y")
summary(model_r1)  
# Igen problemer med sftptv2a6


model_r2 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidb),
                   PITF_data, subset = sample==2 & reg_ind=="Y")
summary(model_r2)  
# Igen problemer med sftptv2a6

model_r3 <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidc),
                   PITF_data, subset = sample==3 & reg_ind=="Y")
summary(model_r3) 
# Igen problemer med sftptv2a6

# output ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(stargazer)
? stargazer

OR.vector <- exp(model_f3$coefficients)


# Reproducere deres model -> de valgte modeller er således (på Goldstones foranledning) f3, c1 og r3
# Goldstone forklare at det er disse der har "mediean accuracy for each type of instabilitt[...]" s. 196
out_PITF <- stargazer(model_f3, model_c1, model_r3, 
                      type = "text", 
                      out="PITF_out.htm",
                      omit = "sftptv2a6",
                      model.numbers = F,
                      column.labels = c("Full Problem Set----------","Civil War Onsets-----","Adverse Regime Change Onsets"))

# Du mangler stadig Odds ration - men det må komme senere.

# Forudsigeler - i forsøg på at finde resultaterne fra table 2
PITF_data$test1 <- predict(model_f3)
head(test1)


