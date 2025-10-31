library(emdbook)
library(ggplot2)
library(readr)
library(tidyverse)


# 1a ----------------------------------------------------------------------

# This model is focused on explaining how often papers are cited as a function of different factors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# age_y is the age, in years, of a publication
# open access is a binary variable that tells whether the paper is free to access (1) or not (0)


#load and reformat the data

citation_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(age_y = 2022-year) %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  ungroup()%>%
  select(r_scripts_available,citations,open_access,age_y) %>%
  na.omit()

# plot if you like 

citation_data %>%
  ggplot(mapping = aes(x = age_y,y = citations))+
  geom_point()

# Fit a full model
#Start by using the poisson to model the data
cites.fit <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int +
                                      rsa*r_scripts_available^d +
                                      oa*open_access^c),
                  start = list(a=0.17,
                               int=1,
                               rsa=0.1,
                               oa=0.1,
                               b=1,
                               c=1,
                               d=1),
                  data = citation_data)

warnings()
# AIC initial
AIC(cites.fit)

#alternatively, run without the r_script_available parameter
cites.fit1 <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int +
                                      oa*open_access^c),
                  start = list(a=0.17,
                               int=1,
                               oa=0.1,
                               b=1,
                               c=1),
                  data = citation_data)
warnings()
AIC(cites.fit1)

#alternatively, run without the open access parameter
cites.fit2 <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int +
                                      rsa*r_scripts_available^d),
                  start = list(a=0.17,
                               int=1,
                               rsa=0.1,
                               b=1,
                               d=1),
                  data = citation_data)
warnings()
AIC(cites.fit2)
#alternatively, run without either the open access or r_scripts_available
cites.fit3 <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int),
                  start = list(a=0.17,
                               int=1,
                               b=1),
                  data = citation_data)
warnings()
AIC(cites.fit3)

#change "a" starting value and leave other parameters in
cites.fit4 <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int +
                                      rsa*r_scripts_available^d +
                                      oa*open_access^c),
                  start = list(a=0.35,
                               int=1,
                               rsa=0.1,
                               oa=0.1,
                               b=1,
                               c=1,
                               d=1),
                  data = citation_data)

AIC(cites.fit4)
#Changing the "a" value didn't change the results of the AIC fit from the 
#original model, so a = 0.17 was a good starting parameter

#Compare them all
AICtab(cites.fit, cites.fit1, cites.fit2, cites.fit3, cites.fit4)
#The first model (cites.fit) included all of the variables (R scripts available,
#age, and open access).The dAIC shows this model being the best fit, despite the 
#inclusion of all parameters.  The inclusion of all the parameters is justified
#since the AIC builds in penalties for adding more parameters. Even though the
#model (cites.fit4) had similar results, changing the start value for "a" did 
#not change the results, so the original model is still the best fit.

# 1b ----------------------------------------------------------------------

# This model is focused on what determines rates of R code sharing by authors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# year is the year of publication (relative to 2010).
# open_access is a binary variable that tells whether the paper is free to access (1) or not (0)
# data_available is a binary variable that tells whether the data are publicly available (1) or not (0)


code_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(data_available = case_when(data_available == "yes" ~ 1,
                                    data_available == "no" ~ 0)) %>%
  
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  mutate(year = year-2010)


# note that for this function I use a logistic transform to ensure the 
#probability stays between 0 and 1 during optimization

sharing.fit <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               d * data_available^e +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               d = 0,
               e = 1,
               o = 0,
               p = 1),
  data = code_data
)
AIC(sharing.fit)
#remove open access in model
sharing.fit2 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               d * data_available^e
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               d = 0,
               e = 1),
  data = code_data
)

AIC(sharing.fit2)

#remove data sharing
sharing.fit3 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               o = 0,
               p = 1),
  data = code_data
)
AIC(sharing.fit3)

#remove both open access and data sharing
sharing.fit4 <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b
                               )),
  start = list(int = 0,
               y = 0,
               b = 1),
  data = code_data
)

AIC(sharing.fit4)

AICtab(sharing.fit, sharing.fit2, sharing.fit3, sharing.fit4)
#In this example sharing.fit2 is the best model based on the dAIC of 0.0, the 
#lowest value.  This model does not use open access as a parameter.  The 
#original model, sharing.fit is also a good fit model and may be statistically
#the same as sharing.fit2. It includes the open access as a parameter, as well 
#as availability of R scripts and publication date.  Because sharing.fit2 is a 
#a simpler model with the lowest dAIC, it remains the best fit.


# 1c ----------------------------------------------------------------------

# This model attempts to explain size variation in the wings of birds
# Wing.length is mean adult wing length
# Mass is mean adult body mass
# Range.size is the area of the geographic range of each species
# Order1 is a categorical variable that lists the taxonomic Order each species fall into.


#new code
library(tidyverse)
library(dplyr)

avonet  <- read.csv("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.csv") %>%
  select(Order1, Wing.Length, Mass, Range.Size) %>%
  na.omit()


avonet %>%
  ggplot(mapping = aes(y=Wing.Length,x=Mass))+
  geom_point()

# Note: there is a lot of data here, it may take a while to fit the full model
library(emdbook)
avonet.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))
warnings()
AIC(avonet.fit)

#remove range size in model
avonet.fit1 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0),
                   data = avonet,
                   parameters = list(int ~ Order1))
AIC(avonet.fit1)

#eliminate intercept for each order
avonet.fit2 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet)
                  
AIC(avonet.fit2)

#keep in intercept and range size, change the functional form of mass to log linear  
avonet.fit3 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass) +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(m = 1,
                                sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))

AIC(avonet.fit3)

#Keep intercept, leave out range size, and have functional form of mass as log linear
avonet.fit4 <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                           m*log(Mass),
                                         sdlog = sd),
                    start = list(m = 1,
                                 sd = 1,
                                 int = 0),
                    data = avonet,
                    parameters = list(int ~ Order1))

AIC(avonet.fit4)
AICtab(avonet.fit, avonet.fit1, avonet.fit2, avonet.fit3, avonet.fit4)
#The model with the best fit in this example is avonet.fit1, which removes 
#the home range parameter from the function.  Eliminating the intercept for each
#order and changing the functional form of mass to log linear made for worse fits
#in the model.  Avonet.fit1 was well supported compared to the other models.  
#The others had dAICs which were much larger than 2.
