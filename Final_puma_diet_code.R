#Assignment 4
#The following data set was used for analysis:

library(tidyverse)
Puma_diet<-read_csv("https://raw.githubusercontent.com/linda-lou248/StatEcol/main/Puma_diet.csv")

#Clean up column title names

install.packages("janitor")
library(janitor)
Puma_diet <- Puma_diet %>% 
  clean_names()
library(dplyr)
library(ggplot2)
library(nnet)


#Change variables to factors to work with, especially for plots, and change 
#Puma sex and other categorical variables to factors.

Puma_diet$puma_sex<-as.factor(Puma_diet$puma_sex)
Puma_diet$puma_id<-as.factor(Puma_diet$puma_id)
Puma_diet$prey_species<-as.factor(Puma_diet$prey_species)
Puma_diet$prey_class<-as.factor(Puma_diet$prey_class)
Puma_diet$season<-as.factor(Puma_diet$season)
Puma_diet$puma_classification<-as.factor(Puma_diet$puma_classification)

# Model 1:  Modeling prey class -----------------------------------------------------

#This model was created using a multinomial logistic regression because the 
#outcome can be one of four categorical outcomes of prey class.  This type of
#generalized linear model was chosen because it can be used to model a data set 
#with both categorical and numerical predictor variables. Puma age, sex, habitat 
#and season were chosen as the predictor variables for prey class.

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

#Run Anova 
install.packages("car")
library(car)
car::Anova(prey_class_model)


#The original prey_class_model, which includes all parameters of puma age, sex, 
#habitat and season provides the best fit for explaining prey class in the puma 
#diet of this data set.  The dAIC. is 0.0 and all the other models are well
#above 2.  



# Model 2:  Modeling number of unique prey species ----------------------------


#The model was created using a generalized linear model with a Poisson 
#distribution.  The GLM was chosen because it can model with both numeric
#and categorical predictor variables, and the Poisson distribution was chosen
#because the response variable is count data, reflecting the number of species 
#killed as a factor of puma age, puma sex, habitat and season.

#Create a new data frame with a new column, n_species, that counts the number
#of distinct species for each unique grouping of variables

species_counts <- Puma_diet %>%
  group_by(puma_id, puma_age, puma_sex, season, puma_classification) %>%
  summarise(n_species = n_distinct(prey_species), .groups = "drop")
#This returns 61 observations of 6 variables

#Create GLM using Poisson distribution
n_species_glm <- glm(n_species ~ puma_age + puma_sex + 
                       puma_classification + season,
                     data = species_counts,
                     family = poisson)


summary(n_species_glm)

#GLM alternatives
#Without puma age
n_species_glm.1 <- glm(n_species ~ puma_sex + puma_classification + season,
                       data = species_counts,
                       family = poisson)

#Without puma sex
n_species_glm.2 <- glm(n_species ~ puma_age + 
                         puma_classification + season,
                       data = species_counts,
                       family = poisson)

#Without puma_classification (habitat)
n_species_glm.3 <- glm(n_species ~ puma_age + puma_sex + 
                         season,
                       data = species_counts,
                       family = poisson)

#Without season
n_species_glm.4 <- glm(n_species ~ puma_age + puma_sex + 
                         puma_classification,
                       data = species_counts,
                       family = poisson)

#Without puma sex and age
n_species_glm.5 <- glm(n_species ~ puma_classification + season,
                       data = species_counts,
                       family = poisson)

AICtab(n_species_glm, n_species_glm.1, n_species_glm.2, n_species_glm.3,
       n_species_glm.4, n_species_glm.5)

#According to the dAIC, the best fit model assumes that the number of each
#species killed is a function of puma classification (habitat) and season.  
#However, the other models are relatively equal except when habitat alone is 
#excluded from the model.  Habitat should necessarily be included in the model.
#This indicates that the number of each species killed
#is dependent on all variables.  


predict(prey_class_model, type = "probs")
preds <- predict(prey_class_model1, type = "class")
table(preds, Puma_diet$prey_species)


# Plotting number of unique prey species ----------------------------------

#The following code plots number of unique species based on puma sex, puma age,
#puma id, habitat and season.

#Examine mean species count by sex
species_counts %>%
  group_by(puma_sex) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#Plot mean species count by sex

ggplot(species_counts, aes(x = factor(puma_sex), y = n_species)) +
  geom_boxplot(fill = "skyblue") +
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

# Plot:  Prey species richness by season and habitat 
ggplot(species_counts, aes(x = puma_classification, y = n_species, 
                          fill = season)) +
                          geom_boxplot() +
                          scale_fill_brewer(palette = "BrBG") +
  labs(x = "Habitat",
       y = "Number of Unique Prey Species") +
  theme_minimal()

#Summarize based on habitat and prey class

prey_puma_class<-Puma_diet[, c("prey_class", "puma_classification")]

summary_prey_puma_class <- prey_puma_class %>%
  group_by(puma_classification, prey_class) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(puma_classification) %>%
  mutate(percent = count / sum(count) *100)

# Plot: Percent of prey classes killed by habitat 

ggplot(summary_prey_puma_class, aes(x = puma_classification, y = percent, fill = prey_class)) +
  geom_bar(position = position_dodge(width = 0.8),
           stat = "summary", fun = "mean") +
          scale_fill_brewer(palette = "BrBG") +
  labs(
    x = "Habitat",
    y = "Percent Killed",
    fill = "Prey Class"
  ) +
  theme_minimal()


#Summarize based on season and prey class

prey_puma_season<-Puma_diet[, c("prey_class", "season")]

summary_prey_puma_season <- prey_puma_season %>%
  group_by(season, prey_class) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(season) %>%
  mutate(percent = count / sum(count) *100)

#Plot:  Percent of prey classes killed by season

ggplot(summary_prey_puma_season, aes(x = season, y = percent, fill = prey_class)) +
  geom_bar(position = position_dodge(width = 0.8), 
           stat = "summary", fun = "mean") +
  scale_fill_brewer(palette = "BrBG") +
  labs(
    x = "Season",
    y = "Percent Killed",
    fill = "Prey Class"
  ) +
  theme_minimal()

# Model 3:  Binomial Model: Prey as Ungulate or Not ----------------------

#This model was created using a binomial distribution based on ungulate or not 
#as prey species.  A generalized linear model was chosen because it can be used 
#to model a data set with both categorical and numerical predictor variables. 
#The binomial distribution was chosen since outcome could be one of two 
#variables - ungulate or not.  Puma age, sex, habitat and season were chosen as 
#the predictor variables to see if the prey species ungulate is a function of 
#these variables.

#Create new data frame with addition of column titled "is_ungulate"
Puma_diet_ungulate <- Puma_diet %>%
  mutate(is_ungulate = ifelse(prey_class == "Ungulate", 1, 0))

#Fit variables from new data frame to binomial GLM.  First model includes all 
#predictor variables.
ungulate_glm <- glm(is_ungulate ~ puma_age + puma_sex + puma_classification 
                    + season,
                    data = Puma_diet_ungulate,
                    family = binomial)

#Call ungulate_glm to look at coefficients
ungulate_glm
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

#Without puma age and puma classification
ungulate_glm.6 <- glm(is_ungulate ~ puma_sex + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma age and season
ungulate_glm.7 <- glm(is_ungulate ~ puma_classification + puma_sex,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma sex and puma classification
ungulate_glm.8 <- glm(is_ungulate ~ puma_age + season,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma sex and season
ungulate_glm.9 <- glm(is_ungulate ~ puma_classification + puma_age,
                      data = Puma_diet_ungulate,
                      family = binomial)

#Without puma classification and season
ungulate_glm.10 <- glm(is_ungulate ~ puma_sex + puma_age,
                       data = Puma_diet_ungulate,
                       family = binomial)

AICtab(ungulate_glm, ungulate_glm.1, ungulate_glm.2, ungulate_glm.3, ungulate_glm.4,
       ungulate_glm.5, ungulate_glm.6, ungulate_glm.7, ungulate_glm.8, 
       ungulate_glm.9, ungulate_glm.10)

#According to the results of AIC comparison of ungulate model fits, the first
#model, ungulate_glm, is the best fit.  This model includes all predictor
#variables and is justified in using all variables based on the delta AIC score.



#Examine the percent of ungulate kills per age group
percent_ungulate_by_age <- Puma_diet_ungulate %>%
  group_by(puma_age) %>% 
  summarise(
    total_kills = n(),
    ungulate_kills = sum(prey_class == "Ungulate"),
    percent_ungulate = (ungulate_kills / total_kills) * 100,
    .groups = "drop"
  )

ggplot(percent_ungulate_by_age, aes(x = factor(puma_age), y = percent_ungulate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Percentage of Ungulate Kills by Puma Age",
    x = "Puma Age",
    y = "Percent Ungulate Kills (%)"
  ) +
  theme_minimal()



