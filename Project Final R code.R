# Install necessary packages in desired location
pkgloc = "/Volumes/Ext2Tb/ExtLibrary/R/library" 
# create list of packages to check
instlist =  c("tidyverse", "haven", "psyntur", "car", "MASS",
              "lavaan", "semPlot", "brms", "rstan","sem")
mypkglst =  rownames(installed.packages())
instpkg = NULL
for (i in 1:length(instlist)) {
  if(!instlist[i] %in% mypkglst) {
    instpkg = c(instpkg,instlist[i])
  }
}
instpkg
# check to make sure list is not NULL, this will throw an error
if(length(instpkg) != 0) install.packages(instpkg,pkgloc)

# Load packages (note: order is important as some packages overwrite others)
library(tidyverse)
library(haven)
library(psyntur)
library(car)
library(MASS)
library(sem)
library(semPlot)
library(brms) # Adjusted 
library(rstan) # for Bayesian analysis
library(dplyr)
library(lavaan) # Load last to avoid altercations


# read in R Data formated dataset.
wd = getwd()
setwd(wd)
load("Berger and Anaki disgust scale 2014 spss.RData")
BA_data = x
remove(wd)


# View dataset
View(BA_data)

# Check for missing values and inspect the data
summary(BA_data)
glimpse(BA_data)
print(BA_data)

# Scrub data, convwert obvious factors from numbers to factors
# convert 3's to NA. (may have been error in code)

# Remove rows with any NA values in Mean_general_ds,Mean_core,Mean_Animal_reminder,Mean_Contamination
BA_data1 <-BA_data %>% drop_na(Mean_general_ds,Mean_core,Mean_Animal_reminder
                               ,Mean_Contamination)

# View data
head(BA_data1)
str(BA_data1)
table(BA_data1$Religion)

# drop factor level 6 ('other') from Religion. This is to prevent problems later
# how: convert Religion to character, then back to factor as there are no 'others'
BA_data1$Religion <- factor(as.character(BA_data1$Religion),order=TRUE,
                       levels = c("secular", "0.5", "observent","1.5","orthodox"))

# Data visualisation
ggplot(BA_data1, aes(x = Religion, y = Mean_core)) +
  geom_point() +
  labs(title = "Religiosity vs Core Disgust")

# Linear regression models - since Religion is an ordinal factor we use
# lm, but indicating that contraxt ="contr.treatment" [default is contr.poly]
# note: contrasts apply only to factors, not real (numeric) variables
#General disgust
model_general_ds <- lm(Mean_general_ds ~ Religion,
                            contrasts=list(Religion="contr.treatment"),data = BA_data1)
summary(model_general_ds)

#Core Disgust
model_core_disgust <- lm(Mean_core ~ Religion,
                         contrasts=list(Religion="contr.treatment"), data = BA_data1)
summary(model_core_disgust)

#Animal Reminder Disgust
model_Animal_reminder <- lm(Mean_Animal_reminder ~ Religion,
                    contrasts=list(Religion="contr.treatment"),data = BA_data1)
summary(model_Animal_reminder)

#Contamination Disgust
model_Contamination <- lm(Mean_Contamination ~ Religion,
                    contrasts=list(Religion="contr.treatment"), data = BA_data1)
summary(model_Contamination)


# SEM model
SEM_model <- '
  # Measurement model
  Religiosity =~ Religion
  Disgust_Sensitivity =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q13 + Q14 + Q15 + Q17 + Q18 + Q19 + Q20 + Q21 + Q22 + Q23 + Q24 + Q25 + Q26 + Q27
  # Structural model
  Disgust_Sensitivity ~ Religiosity
'
fit <- sem(SEM_model, data = BA_data1)
summary(fit, standardized = TRUE)

# Visualise the SEM
semPaths(fit, what = "std", style = "lisrel", layout = "tree")

# Bayesian Regression Model using 'brms'
# note - brm will not correct for assumps of polynomial model if data are ordered ordinal
# Use as.character(Religion) to adjust
b_model_core_disgust <- brm(
  formula = Mean_core ~ as.character(Religion),
  data = BA_data1,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("normal(0, 10)", class = "Intercept")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.95),
  seed = 1234,
  sample_prior = TRUE  # added to correct error later in missing priors
)

# Summary of the Bayesian model
summary(b_model_core_disgust)

# Plot diagnostics

# Posterior predictive checks
pp_check(b_model_core_disgust) 

# Compare predicted values to actual values

pptest <- posterior_predict(b_model_core_disgust, newdata = BA_data1)
qqnorm(pptest[1,])
qqnorm(pptest[7100,])

#Install necessary packages
if("bayesplot" %in% rownames(installed.packages()) == FALSE) {
  install.packages("bayesplot")
}
library(bayesplot)

# Plot the posterior distributions of the parameters
mcmc_areas(b_model_core_disgust)

# Plot the effects of predictors
conditional_effects(b_model_core_disgust)

# Save the model to disk
saveRDS(b_model_core_disgust, file = "b_model_core_disgust.rds")

# Load the model back into R
b_model_core_disgust <-readRDS(file = "b_model_core_disgust.rds")

