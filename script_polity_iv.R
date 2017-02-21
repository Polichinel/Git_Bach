# Polity IV ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

polity_dat <- read.csv("/home/polichinel/Dropbox/KU/6.Semester/Bach/Litteratur_og_data/Polity_IV_data/p4v2015.csv")
polity_dat$sftptv2a <- as.factor(paste(polity_dat$parcomp,polity_dat$exrec, sep = "."))

levels(polity_dat$sftptv2a)
levels(polity_dat$sftptv2a) <- c("FA", "FA","FA","FA","FA","PA","PA","PA", NA,"FA","FA","FA","FA","FA","PA","PA","PA","PA","PA","PA","PD","PD","PD",NA,"PA","PA","PA","PA","PA","PDF","PDF","PDF",NA,"PA","PA","PA","PA","PD","PD","PD",NA,"PA","PA","PA","PD","PD","FD",NA,NA,NA,NA)
levels(polity_dat$sftptv2a) <- c("Full Autocracy","Patial Autocracy", "Partial Democracy", "Partial democracy w/factionalisme", "Full Democracy")
levels(polity_dat$sftptv2a)

# NICE! nu håber man bare det passer... Det skal tjekkes rigtigt mange gange....
# Det er bare super fint. Check lige USA under borger krigen; PDF fra 55, borgerkrig i 61!
# Noter dig at weimer reb er PDF.
# Venezuela er din huckle berry; de er gået fra FD til PDF i 90'erne
  