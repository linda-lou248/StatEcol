#Assignment 3, Part 2
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

install.packages("janitor")
library(janitor)
Puma_diet <- Puma_diet %>% 
  clean_names()

#What I'll be looking at is the number of unique prey species as a function
#of puma age, sex, habitat (puma classification), and season.

#the following is to count unique prey species eaten by each puma
species_counts <- Puma_diet %>%
  group_by(puma_id, puma_age, puma_sex, season, puma_classification) %>%
  summarise(n_species = n_distinct(prey_species), .groups = "drop")
#this returns 61 obs of 6 variables

#get mean species count by sex
species_counts %>%
  group_by(puma_sex) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#get mean species count by age
species_counts %>%
  group_by(puma_age) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#plot
library(ggplot2)
ggplot(species_counts, aes(x = puma_age, y = n_species, fill = puma_sex)) +
  geom_boxplot() +
  labs(title = "Prey Species Richness by Puma Age and Sex",
       x = "Puma Age",
       y = "Number of Unique Prey Species") +
  theme_minimal()

#plot species count break down by individual puma
ggplot(species_counts, aes(x = reorder(puma_id, n_species), y = n_species, fill = puma_sex)) +
  geom_col() +
  labs(title = "Number of Prey Species Eaten per Puma",
       x = "Puma ID",
       y = "Unique Prey Species") +
  coord_flip() +
  theme_minimal()

#mean species counts by season
species_counts %>%
  group_by(season) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#mean species counts by puma classification (habitat)
species_counts %>%
  group_by(puma_classification) %>%
  summarise(mean_species = mean(n_species),
            sd_species = sd(n_species),
            n = n())

#plotting number of unique prey species by season and habitat
library(ggplot2)
ggplot(species_counts, aes(x = puma_classification, y = n_species, fill = season)) +
  geom_boxplot() +
  labs(title = "Prey Species Richness by Puma Classification and Season",
       x = "Habitat",
       y = "Number of Unique Prey Species") +
  theme_minimal()

#created 'dummy variables' for mle2 to handle the factors 
species_counts$puma_sex <- factor(species_counts$puma_sex, 
                                  levels = c("Male", "Female"))
species_counts$season <- factor(species_counts$season, 
                                levels = c("CD", "HD", "HW"))
species_counts$puma_classification <- factor(species_counts$puma_classification, 
                                             levels = c("Riparian", "Upland", "Mixed"))

species_counts$Female <- ifelse(species_counts$puma_sex == "Female", 1, 0)

species_counts$season_HD <- ifelse(species_counts$season == "HD", 1, 0)
species_counts$season_HW <- ifelse(species_counts$season == "HW", 1, 0)

species_counts$Upland <- ifelse(species_counts$puma_classification == "Upland", 1, 0)
species_counts$Mixed  <- ifelse(species_counts$puma_classification == "Mixed", 1, 0)

library(bbmle)
#The first model
puma.fit <- mle2(
  n_species ~ dpois(lambda = exp(int +
                                   b_age * puma_age +
                                   b_female * Female +
                                   b_hotdry * season_HD +
                                   b_hotwet * season_HW +
                                   b_upland * Upland +
                                   b_mixed * Mixed)),
  start = list(int = 0, b_age = 0, b_female = 0,
               b_hotdry = 0, b_hotwet = 0,
               b_upland = 0, b_mixed = 0),
  data = species_counts
)
puma.fit
AIC(puma.fit)

#without season as a factor
puma.fit2 <- mle2(
  n_species ~ dpois(lambda = exp(int +
                                   b_age * puma_age +
                                   b_female * Female +
                                   b_upland * Upland +
                                   b_mixed * Mixed)),
  start = list(int = 0, b_age = 0, b_female = 0,
               b_upland = 0, b_mixed = 0),
  data = species_counts
)
AIC(puma.fit2)

#without habitat as a factor
puma.fit3 <- mle2(
  n_species ~ dpois(lambda = exp(int +
                                   b_age * puma_age +
                                   b_female * Female +
                                   b_hotdry * season_HD +
                                   b_hotwet * season_HW )),
  start = list(int = 0, b_age = 0, b_female = 0,
               b_hotdry = 0, b_hotwet = 0),
  data = species_counts
)
AIC(puma.fit3)

#with habitat and season, but without age and sex
puma.fit4 <- mle2(
  n_species ~ dpois(lambda = exp(int +
                                   b_hotdry * season_HD +
                                   b_hotwet * season_HW +
                                   b_upland * Upland +
                                   b_mixed * Mixed)),
  start = list(int = 0, 
               b_hotdry = 0, b_hotwet = 0,
               b_upland = 0, b_mixed = 0),
  data = species_counts
)
AIC(puma.fit4)
#Comparing all models
AICtab(puma.fit, puma.fit1, puma.fit2, puma.fit3, puma.fit4)

#Now I'd like to look at what of the following explains the prey class of puma diet in this
#data set:  puma sex, age, habitat, and season. 
prey_class.fit <- glm(prey_class ~ puma_age + puma_classification + puma_sex + season,
                family = poisson(link = "log"),
                data = Puma_diet)

summary(prey_class.fit)
library(nnet)
#Here I used the multinomial logistic regression since the outcome, which is
#prey class, has 4 categories
prey_class_model <- multinom(prey_class ~ puma_age + puma_sex + puma_classification + season,
                             data = Puma_diet)
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
AICtab(prey_class_model, prey_class_model1, prey_class_model2, prey_class_model3, prey_class_model4)
#The original prey_class_model, which includes all the parameters of age, sex, 
#habitat and season provides the best fit for explaining prey class of the puma 
#diet in this data set.  The dAIC. is 0.0 and all the other models are well
#above 2.  