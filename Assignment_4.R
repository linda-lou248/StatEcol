#Assignment 4
#The following data set was used for analysis:
read.csv("https://raw.githubusercontent.com/linda-lou248/StatEcol/main/Puma_diet.csv")
Puma_diet<-read_csv("https://raw.githubusercontent.com/linda-lou248/StatEcol/main/Puma_diet.csv")


#change variables to factors to work with, especially for plots
#change Puma sex to factor
Puma_diet$`Puma Sex`<-as.factor(Puma_diet$`Puma Sex`)
Puma_diet$`Puma ID`<-as.factor(Puma_diet$`Puma ID`)
Puma_diet$`Prey Species`<-as.factor(Puma_diet$`Prey Species`)
Puma_diet$`Prey Class`<-as.factor(Puma_diet$`Prey Class`)
Puma_diet$Season<-as.factor(Puma_diet$Season)
Puma_diet$`Puma Classification`<-as.factor(Puma_diet$`Puma Classification`)

library(tidyverse)
library(bbmle)
install.packages("janitor")
library(janitor)
Puma_diet <- Puma_diet %>% 
  clean_names()


# Modeling number of unique prey species ----------------------------------

#In the following code, I'm looking at the number of unique prey species as 
#a function of puma id, puma age, sex, habitat (puma classification), 
#and season.

#Create a new data frame with a new column, n_species, that counts the number
#of distinct species for each unique grouping of variables
species_counts <- Puma_diet %>%
  group_by(puma_id, puma_age, puma_sex, season, puma_classification) %>%
  summarise(n_species = n_distinct(prey_species), .groups = "drop")
#This returns 61 observations of 6 variables

#Examine mean species count by sex
species_counts %>%
  group_by(puma_sex) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#Plot mean species count by sex
library(ggplot2)
ggplot(species_counts, aes(x = factor(puma_sex), y = n_species)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    x = "Puma Sex",
    y = "Species Count",
    title = "Species Count by Puma Sex"
  ) +
  theme_minimal()

#Examine mean species count by age
species_counts %>%
  group_by(puma_age) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#Plot mean species count by age
ggplot(species_counts, aes(x = factor(puma_age), y = n_species)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    x = "Puma Age",
    y = "Species Count",
    title = "Species Count by Puma Age"
  ) +
  theme_minimal()


#Plot species count break down by individual puma
ggplot(species_counts, aes(x = reorder(puma_id, n_species), y = n_species, 
  fill = puma_sex)) +
  geom_col() +
  labs(title = "Number of Prey Species Eaten per Puma",
       x = "Puma ID",
       y = "Unique Prey Species") +
  coord_flip() +
  theme_minimal()

#Examine mean species counts by season
species_counts %>%
  group_by(season) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#Examine mean species counts by puma classification (habitat)
species_counts %>%
  group_by(puma_classification) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#Plot the number of unique prey species by season and habitat
ggplot(species_counts, aes(x = puma_classification, y = n_species, fill = season)) +
  geom_boxplot() +
  labs(title = "Prey Species Richness by Habitat and Season",
       x = "Habitat",
       y = "Number of Unique Prey Species") +
  theme_minimal()


# Modeling prey class -----------------------------------------------------

#This model was created using a multinomial logistic regression because the 
#outcome can be one of four categorical outcomes of prey class.  A 
#generalized linear model was chosen because it can be used to model a data set 
#with both categorical and numerical predictor variables. Puma age, sex, habitat 
#and season were chosen as the predictor variables for prey class.

library(nnet)
#The first model creates prey class as a function of all chosen predictor
#variables.
prey_class_model <- multinom(prey_class ~ puma_age + puma_sex + 
                            puma_classification + season,
                            data = Puma_diet)

summary(prey_class_model)

#Without puma age as factor
prey_class_model1<- multinom(prey_class ~ puma_sex + puma_classification + season,
                             data = Puma_diet)

#Without puma sex as factor
prey_class_model2<- multinom(prey_class ~ puma_age + puma_classification + season,
                             data = Puma_diet)

#Without puma_classification (habitat) as factor
prey_class_model3<- multinom(prey_class ~ puma_age + puma_sex + season,
                             data = Puma_diet)

#Without season as factor
prey_class_model4<- multinom(prey_class ~ puma_sex + puma_classification,
                             data = Puma_diet)

#Without puma age and puma sex as factors
prey_class_model5<- multinom(prey_class ~ puma_classification + season,
                             data = Puma_diet)

#Compare models
AICtab(prey_class_model, prey_class_model1, prey_class_model2, 
       prey_class_model3, prey_class_model4, prey_class_model5)

#####Elaborate and then look at the predict and preds that follow
#The original prey_class_model, which includes all parameters of puma age, sex, 
#habitat and season provides the best fit for explaining prey class in the puma 
#diet of this data set.  The dAIC. is 0.0 and all the other models are well
#above 2.  

predict(prey_class_model, type = "probs")
preds <- predict(prey_class_model1, type = "class")
table(preds, Puma_diet$prey_species)


# Modeling prey species (not number of prey species) ----------------------
#This model was created using a multinomial logistic regression because the 
#outcome can be one of many categorical outcomes which are prey species.  The 
#multinomial model can also be used to model a data set with both categorical 
#and numerical predictor variables. Puma age, sex, habitat and season were 
#chosen as the predictor variables for prey species.  These predictor variables
#were chosen to evaluate their influence on prey selection.


species_multinom <- multinom(prey_species ~ puma_age + puma_sex + puma_classification + season,
                             data = Puma_diet)
summary(species_multinom)

#Without puma age as factor
species_multinom.1 <- multinom(prey_species ~ puma_sex + puma_classification + season,
                             data = Puma_diet)

#Without puma sex as factor
species_multinom.2 <- multinom(prey_species ~ puma_age + puma_classification + season,
                             data = Puma_diet)

#Without puma sex and age as factors
species_multinom.3 <- multinom(prey_species ~ puma_classification + season,
                             data = Puma_diet)

#Without puma classification (habitat) as factor
species_multinom.4 <- multinom(prey_species ~ puma_age + puma_sex + season,
                             data = Puma_diet)

#Without season as factor
species_multinom.5 <- multinom(prey_species ~ puma_age + puma_sex + puma_classification,
                             data = Puma_diet)

#Without puma age and puma classification
species_multinom.6 <- multinom(prey_species ~ puma_sex + season,
                               data = Puma_diet)

#Without puma age and season
species_multinom.7 <- multinom(prey_species ~ puma_sex + puma_classification,
                               data = Puma_diet)

#Without puma sex and puma classification
species_multinom.8 <- multinom(prey_species ~ puma_age + season,
                               data = Puma_diet)
#Without puma sex and season
species_multinom.9 <- multinom(prey_species ~ puma_age + puma_classification,
                               data = Puma_diet)
#Puma classification only
species_multinom.10 <- multinom(prey_species ~ puma_classification,
                               data = Puma_diet)

#Season only
species_multinom.11 <- multinom(prey_species ~ season,
                               data = Puma_diet)

AICtab(species_multinom, species_multinom.1, species_multinom.2, species_multinom.3,
       species_multinom.4, species_multinom.6, species_multinom.5, species_multinom.7,
       species_multinom.8, species_multinom.9, species_multinom.10, species_multinom.11)

#AICtab on these models shows the best fit model to be when prey species is a 
#factor of all predictor variables: puma sex, puma age, habitat and season.  
#The dAIC. is 0.0 for the best fit model.  All the other models are well above 
#a dAIC of 2, which justifies the inclusion of all variables.   


# Modeling # of each species killed ---------------------------------------


library(dplyr)

#Aggregating the data to count kills per species per puma age, puma sex, 
#puma classification, season, prey species
library(dplyr)
species_kills_summary <- Puma_diet %>%
  group_by(puma_age, puma_sex, puma_classification, season, prey_species) %>%
  summarise(kill_count = n(), .groups = "drop")

# View the first few rows
head(species_kills_summary)

# Poisson model
species_kills_model <- glm(kill_count ~ puma_age + puma_sex + 
                             puma_classification + season,
                  data = species_kills_summary,
                  family = poisson)

summary(species_kills_model)

#without age or sex as factors
species_kills_model.1 <- glm(kill_count ~ puma_classification + season,
                           data = species_kills_summary,
                           family = poisson)
summary(species_kills_model.1)

# trying this to see what comes of it - aggregate data to kills per species, per 
library(dplyr)

# Assuming your dataset is called 'kills'
aggregate_kills_summary <- Puma_diet %>%
  group_by(puma_id, puma_sex, puma_age, puma_classification, season, prey_species) %>%
  summarise(kill_count = n(), .groups = "drop")

# View the first few rows
head(aggregate_kills_summary)
#this gives a column with the kill count of each species specific to that season,
#puma id, puma age, habitat, puma sex
#now, because response variable is a count, can run a GLM that is a Poisson or 
#negative binomial that is more typical of a count
# Poisson model
species_kills_pois <- glm(kill_count ~ puma_id + puma_sex + puma_classification 
                          + season + puma_age,
                  data = aggregate_kills_summary,
                  family = poisson)

summary(species_kills_pois)
#without puma id and puma age
species_kills_pois.1 <- glm(kill_count ~ puma_sex + puma_classification + season,
                          data = aggregate_kills_summary,
                          family = poisson)

summary(species_kills_pois.1)

#without puma id, puma sex and puma age
species_kills_pois.2 <- glm(kill_count ~ puma_classification + season,
                            data = aggregate_kills_summary,
                            family = poisson)

summary(species_kills_pois.2)
#check for overdispersion of Poisson model
dispersion <- sum(residuals(species_kills_pois, type = "pearson")^2) / species_kills_pois$df.residual
dispersion

#Output was 2.588773, which indicates overdispersion
###Switch to negative binomial and then check for overdispersion again
####ADD THIS IN


# Binomial Model: Prey as Ungulate or Not ----------------------

#This model was created using a binomial distribution based on ungulate or not 
#as prey species.  A generalized linear model was chosen because it can be used 
#to model a dataset with both categorical and numerica predictor variables.
#Puma age, sex, habitat and season were chosen as the predictor variables 
#to see if ungulate or not ungulate is a function of these variables.

library(dplyr)
library(ggplot2)

#Create new data frame with addition of column titled "is_ungulate"
Puma_diet_ungulate <- Puma_diet %>%
  mutate(is_ungulate = ifelse(prey_class == "Ungulate", 1, 0))

#Fit variables from new dataframe to binomial GLM.  First model includes all 
#predictor variables.
ungulate_glm <- glm(is_ungulate ~ puma_age + puma_sex + puma_classification 
                    + season,
          data = Puma_diet_ungulate,
          family = binomial)

#Call ungulate_glm to look at coefficients
ungulate_glm

#Here I wanted to see what the data looked like for number of 
#ungulate kills per puma age 
ungulate_counts <- Puma_diet_ungulate %>%
  filter(prey_class == "Ungulate") %>%    
  group_by(puma_age) %>%                   
  summarise(ungulate_kills = n(), .groups = "drop")

ungulate_counts

#Plotting the data
ggplot(ungulate_counts, aes(x = puma_age, y = ungulate_kills)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Ungulate Kills by Puma Age",
    x = "Puma Age (years)",
    y = "Number of Ungulate Kills"
      ) +
  theme_minimal()

#Alternatively, look at percentage of ungulate kills per age 
percent_ungulate_by_age <- Puma_diet_ungulate %>%
  group_by(puma_age) %>% 
  summarise(
    total_kills = n(),
    ungulate_kills = sum(prey_class == "Ungulate"),
    percent_ungulate = (ungulate_kills / total_kills) * 100,
    .groups = "drop"
  )

ggplot(percent_ungulate_by_age, aes(x = factor(puma_age), y = percent_ungulate)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Percentage of Ungulate Kills by Puma Age",
    x = "Puma Age",
    y = "Percent Ungulate Kills (%)"
  ) +
  theme_minimal()


summary(ungulate_glm)
#The output of the summary indicates that older pumas, male pumas, Upland 
#habitat, and hot-wet season are all significant predictor variables for 
#ungulate prey.

#Fitting alternative models to test for differences when variables are excluded
#Without puma age
ungulate_glm.1 <- glm(is_ungulate ~ puma_sex + puma_classification + season,
                    data = Puma_diet_ungulate,
                    family = binomial)


#Without puma sex
ungulate_glm.2 <- glm(is_ungulate ~ puma_age + puma_classification + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma classification
ungulate_glm.3 <- glm(is_ungulate ~ puma_sex + puma_age + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without season
ungulate_glm.4 <- glm(is_ungulate ~ puma_sex + puma_classification + puma_age,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma age and puma sex
ungulate_glm.5 <- glm(is_ungulate ~ puma_classification + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#without puma age and puma classification
ungulate_glm.6 <- glm(is_ungulate ~ puma_sex + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#without puma age and season
ungulate_glm.7 <- glm(is_ungulate ~ puma_classification + puma_sex,
                      data = Puma_diet_ungulate,
                      family = binomial)

#without puma sex and puma classification
ungulate_glm.8 <- glm(is_ungulate ~ puma_age + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#without puma sex and season
ungulate_glm.9 <- glm(is_ungulate ~ puma_classification + puma_age,
                      data = Puma_diet_ungulate,
                      family = binomial)

#without puma classification and season
ungulate_glm.10 <- glm(is_ungulate ~ puma_sex + puma_age,
                      data = Puma_diet_ungulate,
                      family = binomial)

AICtab(ungulate_glm, ungulate_glm.1, ungulate_glm.2, ungulate_glm.3, ungulate_glm.4,
       ungulate_glm.5, ungulate_glm.6, ungulate_glm.7, ungulate_glm.8, 
       ungulate_glm.9, ungulate_glm.10)

#According to the results of AIC comparison of ungulate model fits, the first
#model, ungulate_glm, is the best fit.  This model includes all predictor
#variables and is justified in using all variables based on the delta AIC score.