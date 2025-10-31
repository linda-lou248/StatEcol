#Part 2: To give yourself more to work with, you might consider summarizing 
#the data in different ways.  For example, you can calculate the number of 
#different species eaten and see if that depends on age, sex, latitude, etc. 
#See example below (note my variable names are slightly different so you'll 
#need to update a bit)

read.csv("/Users/lindavesser/Documents/Statistical Ecology/REcoStats/Puma_diet.csv")

library(tidyverse)

prey_species_summary <- Puma_diet %>%
  group_by('Puma ID`,`Puma Sex`,`Puma Age`) %>%
  summarise(prey_species = length(unique(`Prey Species`)))
prey_species_summary
plot(prey_species_summary$prey_species ~ prey_species_summary$`Puma Age`)

cor.test(prey_species_summary$prey_species,prey_species_summary$`Puma Age`)

dev.off()

