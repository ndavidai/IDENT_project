### Data cleaning ###

#### egg data ####
#loading data 
egg_mass_counts <- read.csv("input/IDENT_oviposition.csv")

# quick visualizations
summary(egg_mass_counts)
str(egg_mass_counts)
# change tree species, quadr color, plot code, tree height to 'factor'; change DBH, area of masses, total area, est num of eggs into 'numerical'
unique(egg_mass_counts$Number.of.Egg.Masses)
unique(egg_mass_counts$Areas.of.egg.masses)
# remove mistakes in [1, 12]
unique(egg_mass_counts$Total.Area.estimate)
unique(egg_mass_counts$Estimated.number.of.eggs)

## there are 6 columns with nothing on them (X, X.1 and X.2...)- let's remove them ##
library(dplyr)
egg_mass_counts_1 <-egg_mass_counts %>% select(1:11)

unique(egg_mass_counts_1$Number.of.Egg.Masses)
unique(egg_mass_counts_1$Areas.of.egg.masses)
# remove mistakes in [1, 12]

#[1] "" [12]  extra space at the end
## there are some rows that are completely empty- we want to replace those with 0##
# replace empty cells with 0
egg_mass_counts_1 <- replace(egg_mass_counts_1, egg_mass_counts_1=='', 0)

#remove extra space at the end
egg_mass_counts_1 <- replace(egg_mass_counts_1, egg_mass_counts_1=='D + D + D + D + D + D + D + D + D + D ', "D + D + D + D + D + D + D + D + D + D")

unique(egg_mass_counts_1$Number.of.Egg.Masses)
unique(egg_mass_counts_1$Areas.of.egg.masses)

library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
egg_mass_counts_1 <- egg_mass_counts_1 %>%
  clean_names() #Cleans names of an object (usually a data.frame)

unique(egg_mass_counts_1$dbh_diameter_cm)
#replace "None" wtih "NA"
egg_mass_counts_1 <- replace(egg_mass_counts_1, egg_mass_counts_1=='None', 'NA')
unique(egg_mass_counts_1$dbh_diameter_cm)
unique(egg_mass_counts_1$quadrant_color)
unique(egg_mass_counts_1$plot_code)

#visualize data
#library(esquisse)
#esquisser(egg_mass_counts_1)

library(esquisse)
esquisser(egg_mass_counts_1)


library(dplyr)
library(ggplot2)

egg_mass_counts_1 %>%
 filter(!(tree_species %in% c("Red oak ", "Red"))) %>%
 ggplot() +
 aes(x = tree_species, y = number_of_egg_masses) +
 geom_col(fill = "#BF661A") +
 theme_minimal() +
 theme(axis.title.y = element_text(size = 18L, face = "bold"), axis.title.x = element_text(size = 18L, 
 face = "bold"))
library(ggplot2)

ggplot(egg_mass_counts_1) +
 aes(x = number_of_egg_masses, y = tree_species) +
 geom_boxplot(fill = "#CD1EB5") +
 theme_minimal()
library(ggplot2)

ggplot(egg_mass_counts_1) +
 aes(x = number_of_egg_masses, y = tree_species) +
 geom_boxplot(fill = "#B9228B") +
 theme_classic()
library(dplyr)
library(ggplot2)

write.csv(egg_mass_counts_1, "IDENT_oviposition_1.csv")






### Extra code from Riikka's original script ###
egg_counts_1 <- replace(egg_counts_1, egg_counts_1==',', NA)
# replace "no entry" with NA
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='no entry', NA)
# replace "none" with NA ? maybe not as this seems categorical
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='none', 0)
unique(egg_counts_1$Egg.Count)
unique(egg_counts_1$Caterpillar.Count)

# if any column names need replacing
colnames(egg_counts_1)[colnames(egg_counts_1)=="Site.Name"] <- "Site_Name"

#### caterpillar data ####
#loading data 
caterp_counts <- read.csv("MSB Caterpillar Counts.csv")

# quick visualizations
summary(caterp_counts)
str(caterp_counts)
unique(caterp_counts$Egg.Count)
unique(caterp_counts$Site.Name)

#library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
caterp_counts <- caterp_counts %>%
  clean_names() #Cleans names of an object (usually a data.frame) 

## let's remove notes column ##
library(dplyr)
caterp_counts_1 <-caterp_counts %>% select(-13)
# let's check for inconsistencies in names etc
unique(caterp_counts_1$dbh_cm)
unique(caterp_counts_1$ )

## there are some rows that are completely empty- we want to replace those with NA##
# replace empty cells with NA
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='', NA)
# replace "no entry" with NA (or with something else?)
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='no entry', NA)
# replace "none" with NA ? maybe not as this seems categorical
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='none', 0)

unique(caterp_counts_1$dbh_cm) #see "no entry "
# replace "no entry " with NA (or with something else?)
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='no entry ', NA)
unique(caterp_counts_1$tree_id) #you may want to change these to separate common name and scientific name columns

## replace space between words with underscore
library(tidyverse)
caterp_counts_1 <- caterp_counts_1 %>%
  mutate(tree_id = str_replace(tree_id, " ", "_"))

# replace "none " with "none"
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='none ', "none")

summary(caterp_counts_1)
str(caterp_counts_1)

## remove % and replace with nothing
caterp_counts_1 <- caterp_counts_1 %>%
  mutate(humidity = str_replace(humidity, "%", ""))

# if any column names need replacing
colnames(caterp_counts_1)[colnames(caterp_counts_1)=="humidity"] <- "humidity_percent"

## some numeric columns are character here- let's change them
caterp_counts_1$dbh_cm <- as.numeric(caterp_counts_1$dbh_cm)
caterp_counts_1$humidity_percent <- as.numeric(caterp_counts_1$humidity_percent)

summary(caterp_counts_1)
str(caterp_counts_1)

egg_mass_counts_1 %>%
  filter(!is.na(temp_c)) %>%
  filter(!is.na(humidity_percent)) %>%
  filter(!is.na(in_center)) %>%
  filter(!is.na(count_height)) %>%
  filter(!is.na(egg_count)) %>%
  filter(!is.na(caterpillar_count)) %>%
  filter(!is.na(entered_by)) %>%
  ggplot() +
  aes(x = dbh_cm, y = egg_count) +
  geom_boxplot(fill = "#2FD6BE") +
  scale_x_continuous(trans = "log") +
  coord_flip() +
  theme_minimal()
