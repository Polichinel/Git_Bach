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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MODELLER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# outputs for modellerne f3, c1 og r3 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(stargazer)
?stargazer

# Reproducere deres model -> de valgte modeller er således (på Goldstones foranledning) f3, c1 og r3
# Goldstone forklare at det er disse der har "mediean accuracy for each type of instabilitt[...]" s. 196
out_PITF <- stargazer(model_f3, model_c1, model_r3,
                      covariate.labels = c("Partial Autocracy",
                                           "Partial Democracy with Factionalisme",
                                           "Partial democracy without Factionalism",
                                           "Full Democracy",
                                           "Infant Mortality",
                                           "Armed Conflict in 4+ Bordering States",
                                           "State-Led Discrimination"),
                      type = "text", 
                      out="PITF_out.htm",
                      omit = "sftptv2a6",
                      model.numbers = F,
                      column.labels = c("Full Problem Set----------","Civil War Onsets-----","Adverse Regime Change Onsets"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Med odds ratio i stedet for log odds (kun for model f3 indtil vidre) ++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Konstruere odds ratio (til stargazer de findes allerede i summmary)
model_f3_or <- exp(model_f3$coefficients) # odds ratio mf3
model_c1_or <- exp(model_c1$coefficients) # odds ratio mc1
model_r3_or <- exp(model_r3$coefficients) # odds ratio mr3

# Konstruere korrekte ci til odds ratio
model_f3_ci  <- exp(confint(model_f3))
model_c1_ci  <- exp(confint(model_c1))
model_r3_ci  <- exp(confint(model_r3))

# Reproducere deres model -> nu (kun) med odds ratio..
out_PITF_or <- stargazer(model_f3, model_c1, model_r3,
                      coef = list(model_f3_or,model_c1_or,model_r3_or),
                      ci.custom = list(model_f3_ci, model_c1_ci, model_r3_ci),
                      covariate.labels = c("Partial Autocracy",
                                           "Partial Democracy with Factionalisme",
                                           "Partial democracy without Factionalism",
                                           "Full Democracy",
                                           "Infant Mortality",
                                           "Armed Conflict in 4+ Bordering States",
                                           "State-Led Discrimination"),
                      p.auto = F,
                      type = "text", 
                      out="PITF_out_or.htm",
                      omit = "sftptv2a6",
                      model.numbers = F,
                      column.labels = c("Full Problem Set----------","Civil War Onsets-----","Adverse Regime Change Onsets"))

# nu INGEN problemer med p-værdierne, efter p.auto = F
# Spledes en model med odds ratio og korrekte ci; men stadigt ikke samlet model...
#----------------------------------------
# Nu kun med model_r3 (ARC) samlet:
summary(model_r3)$coefficients[,3]
model_r3_se <- summary(model_r3)$coefficients[,3]


out_PITF_or <- stargazer( model_r3, model_r3,
                         coef = list(model_r3_or),
                         ci.custom = list(model_r3_ci),
                         dep.var.labels = "Adverse Regime Change Onsets",
                         covariate.labels = c("Partial Autocracy",
                                              "Partial Democracy with Factionalisme",
                                              "Partial democracy without Factionalism",
                                              "Full Democracy",
                                              "Infant Mortality",
                                              "Armed Conflict in 4+ Bordering States",
                                              "State-Led Discrimination"),
                         p.auto = F,
                         type = "text", 
                         out="PITF_out_or_r3.htm",
                         omit = "sftptv2a6",
                         model.numbers = F,
                         column.labels = c("Odds ratio(95% CI)","Coef. (S.E.)"))

# Stadig problemer med at få se ind..


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modeller f3 med FD som ref.cat. ++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
model_f3_rc_fd <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a1 +  # nu full auto a1 i stedet for full demo a2
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidc),
                   PITF_data, subset = sample==3 & glb_ind=="Y")

stargazer(model_f3_rc_fd,
          covariate.labels = c("Partial Autocracy",
                               "Partial Democracy with Factionalisme",
                               "Partial democracy without Factionalism",
                               "Full Autocracy",
                               "Infant Mortality",
                               "Armed Conflict in 4+ Bordering States",
                               "State-Led Discrimination"),
          type = "text", 
          out="PITF_out_rc_fd.htm",
          omit = "sftptv2a6")
# Her ser vi at kun PDF er sig anderledes fra FD (***) - det er fint og vigtigt.
# Men inft. mort, conflikt in.., og discr. har self ikke ændret sig.
# OR og ci:

model_f3_rc_fd_or <- exp(model_f3_rc_fd$coefficients) # odds ratio mf3_rc_df
model_f3_rc_fd_ci  <- exp(confint(model_f3_rc_fd))

model_f3_rc_fd_or
model_f3_rc_fd_ci

# for pdf odds ratio på 13.85 og ci på 3.45 - 55.53. 
# dvs. oddsne for at at et PDF falder er altså 3 til 55 gange større end i et FD
# Bedste bud er er oddsne er 13.85 gange større i PDF frem for FD
# Stop.

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modeller c1 med FD som ref.cat. ++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_c1_rc_fd <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a1 +  # nu full auto a1 i stedet for full demo a2
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratida),
                   PITF_data, subset = sample==1 & cwar_ind=="Y")

stargazer(model_c1_rc_fd,
          covariate.labels = c("Partial Autocracy",
                               "Partial Democracy with Factionalisme",
                               "Partial democracy without Factionalism",
                               "Full Autocracy",
                               "Infant Mortality",
                               "Armed Conflict in 4+ Bordering States",
                               "State-Led Discrimination"),
          type = "text", 
          out="PITF_out_rc_fd.htm",
          omit = "sftptv2a6")

# Samme resultat som f3

model_c1_rc_fd_or <- exp(model_c1_rc_fd$coefficients) # odds ratio mc1_rc_df
model_c1_rc_fd_ci  <- exp(confint(model_c1_rc_fd))

model_c1_rc_fd_or
model_c1_rc_fd_ci

# for pdf odds ratio på 16.52 og ci på 2.2 - 123.57. 
# dvs. oddsne for borgerkrig i PDF er altså 2 til 123 gange større end i et FD
# Bedste bud er er oddsne er 16.51 gange større i PDF frem for FD

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modeller r3 med FD som ref.cat. ++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_r3_rc_fd <- clogit(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a1 + # nu full auto a1 i stedet for full demo a2
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                     disp4cat + 
                     strata(stratidc),
                   PITF_data, subset = sample==3 & reg_ind=="Y")

stargazer(model_r3_rc_fd,
          covariate.labels = c("Partial Autocracy",
                               "Partial Democracy with Factionalisme",
                               "Partial democracy without Factionalism",
                               "Full Autocracy",
                               "Infant Mortality",
                               "Armed Conflict in 4+ Bordering States",
                               "State-Led Discrimination"),
          type = "text", 
          out="PITF_out_r3_rc_fd.htm",
          omit = "sftptv2a6")

# Samme resultat som f3 og c1

model_r3_rc_fd_or <- exp(model_r3_rc_fd$coefficients) # odds ratio mr3_rc_df
model_r3_rc_fd_ci  <- exp(confint(model_r3_rc_fd))

model_r3_rc_fd_or
model_r3_rc_fd_ci

# for pdf odds ratio på 44.69 og ci på 4 - 499.31. 
# dvs. oddsne for adverse regimechange i PDF er altså 4 til 500 gange større end i et FD
# Bedste bud er er oddsne er 44.69 gange større i PDF frem for FD

# stop++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# kan du lave output med alle de tre ny modeller med korrekte p?
# GOOOOO!
stargazer(model_f3_rc_fd, model_c1_rc_fd, model_r3_rc_fd,
         covariate.labels = c("Partial Autocracy",
                              "Partial Democracy with Factionalisme",
                              "Partial democracy without Factionalism",
                              "Full Autocracy",
                              "Infant Mortality",
                              "Armed Conflict in 4+ Bordering States",
                              "State-Led Discrimination"),
         dep.var.labels = "Odds Ratio regarding:",
         dep.var.caption = "",
         coef = list(model_f3_rc_fd_or,model_c1_rc_fd_or,model_r3_rc_fd_or),
         ci.custom = list(model_f3_rc_fd_ci, model_c1_rc_fd_ci, model_r3_rc_fd_ci),
         p.auto = F,
         type = "text", 
         out="PITF_out_rc_fd_or.htm",
         omit = "sftptv2a6",
         model.numbers = F,
         column.labels = c("Full Problem Set----------","Civil War Onsets-----","Adverse Regime Change Onsets"))

# OR er korrekte og nu også ci. og p efter p.auto = F
# Modellen er (substansielt set) færdig. nu layout..

# Konstruere din trip, trap træsko model:
# Nu ARC med kun regime type:------------------------------

model_r3_rc_fd_rt <- clogit(sftpcons ~ sftptv2a3 + 
                           sftptv2a4 + 
                           sftptv2a5 + 
                           sftptv2a1 + # nu full auto a1 i stedet for full demo a2
                           sftptv2a6 + 
                           strata(stratidc),
                         PITF_data, subset = sample==3 & reg_ind=="Y")
summary(model_r3_rc_fd_rt)
# CI og og OR:

model_r3_rc_fd_rt_or <- exp(model_r3_rc_fd_rt$coefficients) # odds ratio mr3_rc_df
model_r3_rc_fd_rt_ci  <- exp(confint(model_r3_rc_fd_rt))

# Nu ARC med regime type og inf. mort.-------------------------------
model_r3_rc_fd_im <- clogit(sftpcons ~ sftptv2a3 + 
                           sftptv2a4 + 
                           sftptv2a5 + 
                           sftptv2a1 + # nu full auto a1 i stedet for full demo a2
                           sftptv2a6 + 
                           logim + 
                           strata(stratidc),
                         PITF_data, subset = sample==3 & reg_ind=="Y")

# CI og og OR:

model_r3_rc_fd_im_or <- exp(model_r3_rc_fd_im$coefficients) # odds ratio mr3_rc_df
model_r3_rc_fd_im_ci  <- exp(confint(model_r3_rc_fd_im))

# Nu kun ARC:-------------------------------------------------
stargazer(model_r3_rc_fd_rt, model_r3_rc_fd_im, model_r3_rc_fd,
          covariate.labels = c("Partial Autocracy",
                               "Partial Democracy with Factionalisme",
                               "Partial democracy without Factionalism",
                               "Full Autocracy",
                               "Infant Mortality",
                               "Armed Conflict in 4+ Bordering States",
                               "State-Led Discrimination"),
          dep.var.labels = "Odds Ratio regarding:",
          dep.var.caption = "",
          coef = list(model_r3_rc_fd_rt_or,model_r3_rc_fd_im_or,model_r3_rc_fd_or),
          ci.custom = list(model_r3_rc_fd_rt_ci,model_r3_rc_fd_im_ci,model_r3_rc_fd_ci),
          p.auto = F,
          type = "text", 
          out="PITF_out_r3_fd_or.htm",
          omit = "sftptv2a6",
          model.numbers = F,
          column.labels = c(""))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# ------------------- relativ risks ------------ et desparat forsøg...
exp(3.61)/(1+exp(3.61))



exp(3.8)/(1+exp(3.8))
# = 0.9781187

summary(model_r3_rc_fd)
exp(0+3.8*1)

44.690/(1+44.690)
3.800*(0.9781134/1+0.9781134)*(1/1+0.9781134)


3.800*0.9781134*(1-0.9781134)


16.520/(1+16.520)

?clogit

#beta = 3.800

#odds ratio = 44.690

3.800*(44.690/1+44.690)*(1/1+44.690)

? mfx

logitmfx(model_r3, data = PITF_data)

#-----------------------------


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



# AME +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(mfx)
??mfx
logitmfx(model_f3,data = PITF_data)

model_t <- glm(sftpcons ~ sftptv2a3 + 
                     sftptv2a4 + 
                     sftptv2a5 + 
                     sftptv2a2 + 
                     sftptv2a6 + 
                     logim + 
                     maccat + 
                   disp4cat, family = binomial, data = PITF_data)
summary(model_t)
logitmfx(sftpcons ~ sftptv2a3 + 
           sftptv2a4 + 
           sftptv2a5 + 
           sftptv2a2 + 
           sftptv2a6 + 
           logim + 
           maccat + 
           disp4cat, data = PITF_data)
# hmmm +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# rest til ci - måske det skal bruges.
## cbind(OR = OR.vector, CI.vector)
## stargazer(mylogit, coef = list(OR.vector), ci = T, 
## ci.custom = list(CI.vector), single.row = T, type = "text")

# Nu skal de bare "merges" -> og der er stadigt lidt med layoutet..
# Du skal havde demokrati som ref.cat
# Du skal havde AME

# Fulde demokratier der har oplevet konflkiter: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# fuld dem med ustabilitet
konflikt_fd <- subset(PITF_data, PITF_data$sftptv2a2 == 1 & PITF_data$sftpcons == 1 & PITF_data$sample == 3)
konflikt_fd$sftgname
# "France", "Gambia", "Laos", "Malaysia", "Papua New Guinea", "United Kingdom", "Uruguay"   

# Borgerkrige:
bogerkrige_fd <- subset(konflikt_fd, konflikt_fd$cwar_ind == "Y")

# Adverse regime changes:
Adverse_rc_fd <- subset(konflikt_fd, konflikt_fd$reg_ind == "Y")

# Super. Men -> hvilke af disse er endt som andet end demokratier efter (burde være alle med adveres rc, men...)
# Hvor er venesuela og Chile? er de kun delvise demokratier?
# Chile ses i dette dataset som pdf, men Venezuela gik fra pd til fd et sted mellem 1970 0g 76.. Hvornår var det sidste fald.
# Det kunne klæde dig godt med en omskrivning af Polity IV 2015 data..

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Forudsigeler (lige nu rod) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


sub_s3 <- subset(PITF_data, PITF_data$sample == 3)
sub_s3$pred <- predict(model_f3) # forudsagte sansynligheder
model_f3_or <- exp(model_f3$coefficients) # odds ratio.



sort(sub_s3$or)

out_of_s <- subset(sub_s3, sub_s3$year > 1994)
sort(out_of_s$pred, decreasing = T)




#..+++++++++++++

















#  i forsøg på at finde resultaterne fra table 2
test_df <- subset(PITF_data, sample == 1)


# Det kunne du hav gjort meget tidligere -> det kan være du skal gå tilbage og gøre det..

# Konstruere predict værdier
test_df$test_predict <- predict(model_f3)
test_df2 <- subset(test_df, test_df$year > 1994)

sort(test_df$test_predict, decreasing = T, test_df$year > 1994)

str(test_df)
# tænke pause





test_fd_us1 <- subset(test_df, test_df$sftptv2a2 == 1 & test_df$sftpcons == 1)


?predict


test_us <- subset(test_df, test_df$test_predict > 0.2300 & test_df$sftpcons == 1)

test_df$test_predict2 <- predict(model_f3)

str(model_f3)

test_df$test_residuals <- residuals(model_f3)

test_df$test_OR <- exp(as.numeric(model_f3$test_predict2))


test_df$test_OR <- exp(model_f3$coefficients)

(model_f3)




m <- exp(44.69)/(1+exp(44.69))

3.8*44.69*(1-44.69)

