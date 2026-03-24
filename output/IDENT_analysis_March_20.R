
library(janitor)                       
library(dplyr)
library(ggplot2)
library(esquisse)
library(ordinal)
library(tidyr)
library(ordinal)
library(DHARMa)
library(Matrix)
library(lme4)
library(glmmTMB)
library(forcats)
library(car)
library(corrplot)

#loading data 
IDENT_data <- read.csv("input/IDENT_oviposition_clean.csv")

summary(IDENT_data)
str(IDENT_data)
unique(IDENT_data$tree_species)

# combine all 'Red oak' variations
IDENT_data <- replace(IDENT_data, IDENT_data =='Red','Red oak')
IDENT_data <- replace(IDENT_data, IDENT_data =='Red oak ','Red oak')

unique(IDENT_data$tree_species)

##check data structure
table(IDENT_data$tree_species, IDENT_data$quadrant_color)
table(IDENT_data$number_of_egg_masses, IDENT_data$tree_height)

names(IDENT_data)[6] <- "tree_height"

#change total area estimate from 'number' to 'integer'
IDENT_data$total_area_estimate <- as.integer(IDENT_data$total_area_estimate)
str(IDENT_data)

# Data visualization ------------------------------------------------------

#visualize data

#library(esquisse)
#esquisser(IDENT_data)

##graph # of egg masses by trees
IDENT_data %>%
 ggplot() +
 aes(x = tree_species, y = number_of_egg_masses) +
 geom_col(fill = "#BF661A") +
 theme_minimal() +
 theme(axis.title.y = element_text(size = 18L, face = "bold"), axis.title.x = element_text(size = 18L, 
 face = "bold"))

ggplot(IDENT_data) +
 aes(x = number_of_egg_masses, y = tree_species) +
 geom_boxplot(fill = "#B9228B") +
 theme_classic()

## graph area of egg masses (cm2) by tree

IDENT_data %>%
  ggplot() +
  aes(x = tree_species, y = total_area_estimate) +
  geom_col(fill = "#BF661A") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 18L, face = "bold"), axis.title.x = element_text(size = 18L, 
                                                                                            face = "bold"))

ggplot(IDENT_data) +
  aes(x = total_area_estimate, y = tree_species) +
  geom_boxplot(fill = "#B9228B") +
  theme_classic()


# mixed effects models ---------------------------------------------------

m_count <- glmmTMB(
  number_of_egg_masses ~ tree_species + tree_height + (1 | plot_code) +
    (1 | quadrant_color),
  family = nbinom2,
  data = IDENT_data
)
summary(m_count)

sim_res <- simulateResiduals(m_count)
plot(sim_res)
testDispersion(sim_res)

testZeroInflation(sim_res)
testUniformity(sim_res)
#Dispersion	p = 0.84 - Good; Zero-inflation	p = 0.888 - not present;
#Uniformity	p = 0.859 - Good

m_count_interaction <- glmmTMB(
  number_of_egg_masses ~ tree_species * tree_height + (1 | plot_code) +
    (1 | quadrant_color),
  family = nbinom2,
  data = IDENT_data
)
summary(m_count_interaction)

sim_res_2 <- simulateResiduals(m_count_interaction)
plot(sim_res_2)
testDispersion(sim_res_2)

testZeroInflation(sim_res_2)
testUniformity(sim_res_2)
#Dispersion	p = 0.58 - slight underdispersion, no real issue; 
#Zero-inflation	p = 0.84 - not present;
#Uniformity	p = 0.886 - Good

m_area <- glmmTMB(
  total_area_estimate ~ tree_species + tree_height + (1 | plot_code) +
    (1 | quadrant_color),
  family = nbinom2,
  data = IDENT_data
)
summary(m_area)

sim_res_3 <- simulateResiduals(m_area)
plot(sim_res_3)
testDispersion(sim_res_3)

testZeroInflation(sim_res_3)
testUniformity(sim_res_3)


m_area_interaction <- glmmTMB(
  total_area_estimate ~ tree_species * tree_height + (1 | plot_code) +
    (1 | quadrant_color),
  family = nbinom2,
  data = IDENT_data
)
summary(m_area_interaction)

sim_res_4 <- simulateResiduals(m_area_interaction)
plot(sim_res_4)
testDispersion(sim_res_4)

testZeroInflation(sim_res_4)
testUniformity(sim_res_4)

# check for correlation between variables
cor(IDENT_data[, c("number_of_egg_masses" , "total_area_estimate")], 
    use = "complete.obs")

cor_mat <- cor(IDENT_data[, c("number_of_egg_masses" , "total_area_estimate")], 
               use = "complete.obs")
corrplot(cor_mat, method = "circle")

# Association between categorical predictors
chisq.test(IDENT_data$tree_species,
           IDENT_data$tree_height)

fisher.test(IDENT_data$tree_species,
            IDENT_data$tree_height)

# Multicollinearity
vif(lm(number_of_egg_masses ~ tree_species + tree_height + quadrant_color,
       data = IDENT_data))

#all predictors above are essentially uncorrelated, so all can be used in 
#a model, as well as their interaction terms
