# Script in which you should do your analysis described on Canvas

library(dplyr)
library(ggplot2)
library(knitr) 

#loading data
data("BCG", package = "HSAUR")
data("birthwt", package = "MASS")
data("bdendo", package = "Epi")

# TUBERCULOSIS

# adding columns
BCG$Vacc_NoTB <- BCG$BCGVacc - BCG$BCGTB
BCG$NoVacc_NoTB <- BCG$NoVacc - BCG$NoVaccTB

# calculating odds ratio
BCG$oddsRatio <- (BCG$BCGTB/BCG$NoVaccTB)/(BCG$Vacc_NoTB/BCG$NoVacc_NoTB)
oddsRatio <- BCG$oddsRatio

# plots
oddRatio_plot <- hist(oddsRatio, col='blue', main= "Odds ratios of BCG vaccine and given the occurance of TB")

latitudes <- BCG$Latitude
years <- BCG$Year

# calculating relative risk 
BCG$relative_ratios <- (BCG$BCGTB/BCG$BCGVacc) / (BCG$NoVaccTB/BCG$NoVacc)

# data for risk vs odds ratio
data_for_plot <- BCG %>% 
  select(relative_ratios, oddsRatio)

#######################################################
#######################################################

# LOW BIRTH WEIGHT

# mean birth weight
mean_bwt <- birthwt %>% 
  summarise(mean = mean(bwt))

# mean weight of mothers
mean_mwt <- birthwt %>% 
  summarise(mean = mean(lwt))

# proportion of low birth weights
low_bwt_number <- nrow(filter(birthwt, low == 0))
low_bwt_proportion <- low_bwt_number/ (nrow(birthwt) - low_bwt_number)

# proportion of smokers
smokers_number <- nrow(filter(birthwt, smoke == 1))
smokers_proportion <- smokers_number/ (nrow(birthwt) - smokers_number)

# median of first trimester visits 
median_visits <- birthwt %>% 
  summarise(median = median(ftv))

# function to calculate relative risk
relative_risk <- function(col) {
  numerator <- nrow(filter(birthwt, col == 1 & low == 1))/ nrow(filter(birthwt, col == 1))
  denominator <- nrow(filter(birthwt, col == 0 & low == 1))/ nrow(filter(birthwt, col == 0))
  return (numerator/denominator)
}

# relative risk based on input variable
smoke_rr <- relative_risk(birthwt$smoke)

ht_rr <- relative_risk(birthwt$ht)

birthwt$no_visits <- ifelse(birthwt$ftv == 0, 1, 0)
ftv_rr <- relative_risk(birthwt$no_visits)

birthwt$under_20 <- ifelse(birthwt$age < 20, 1, 0)
age_rr <- relative_risk(birthwt$under_20)

# a dataframe for the values of relative risks 
input_variable <- c("Smoking Status","Hypertension Status", "Attending 0 prenatal care visits", "Giving birth before age 20")
relative_risks <- c(smoke_rr, ht_rr, ftv_rr, age_rr)
df <- data.frame(input_variable, relative_risks)

#######################################################
#######################################################

## ENDOMETRIAL CANCER

# hypertension 
hypertension_status = epitab(bdendo$d, bdendo$hyp, method = "oddsratio")$tab[2,5]

#obesity
obesity_status = epitab(bdendo$d, bdendo$ob, method = "oddsratio")$tab[2,5]

#gallbladder
gallbladder_status = epitab(bdendo$d, bdendo$gall, method = "oddsratio")$tab[2,5]