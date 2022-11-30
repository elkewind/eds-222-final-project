### ------------------- EDS 222 Final Project ------------------- ###
### Identifying key traits in Hawaiian fish to predict risk of extinction ###

# Load libraries
library(rfishbase)
library(tidyverse)
library(janitor)
library(jsonlite)
library(rredlist)

### -------------------------- FishBase --------------------------  ###

# Load all species
species <- fb_tbl("species") %>% 
  janitor::clean_names()

# Load all species by country
country <- fb_tbl("country") %>% 
  janitor::clean_names()

# Join country with species info
species <- left_join(country, species, by = "spec_code")

# Concatenate Genus and Species
species$genus_species <- paste(species$genus, species$species)
species <- species %>% relocate(genus_species, .after = spec_code) %>% 
  relocate(length, weight, .after = genus_species)
species_list <- as.character(species$genus_species) # Isolate list of species

# Check out what is available in the various tables
tables <- docs()
ecology <- fb_tbl("ecology")
com_names <- fb_tbl("comnames")

# Grab the ecological traits I'm looking for
ecol <- ecology(my_species_list = species_list, 
                   fields = c("SpecCode", "CoralReefs", "FeedingType", "Schooling")) %>% 
  clean_names() # Theres duplicates here but I remove them later
com_names <- common_names(species_list = species_list,
                             fields = c("SpecCode", "ComName")) %>%
  clean_names()

# Combine data sets then clean
fish_chars <- left_join(species, ecol, by = "spec_code") %>% 
  select(c("spec_code", "genus_species", "length", "weight", 
           "status", "current_presence", "genus", "species", "importance.y", 
           "price_categ", "coral_reefs", "feeding_type", "schooling")) %>% 
  filter(current_presence == "present") %>% # Data frame full of fish characteristics
  rename(length_cm = length)

### --------------------------  IUCN --------------------------  ###

# Identify token for accessing IUCN API
iucn_token <- Sys.getenv("IUCN_KEY")

# Import all species on IUCN Redlist
species0 <- rl_sp(page = 0, key = iucn_token)
species0_df <- species0$result
species1 <- rl_sp(page = 1, key = iucn_token)
species1_df <- species1$result
species2 <- rl_sp(page = 2, key = iucn_token)
species2_df <- species2$result
species3 <- rl_sp(page = 3, key = iucn_token)
species3_df <- species3$result
species4 <- rl_sp(page = 4, key = iucn_token)
species4_df <- species4$result
species5 <- rl_sp(page = 5, key = iucn_token)
species5_df <- species5$result
species6 <- rl_sp(page = 6, key = iucn_token)
species6_df <- species6$result
species7 <- rl_sp(page = 7, key = iucn_token)
species7_df <- species7$result
species8 <- rl_sp(page = 8, key = iucn_token)
species8_df <- species8$result
species9 <- rl_sp(page = 9, key = iucn_token)
species9_df <- species9$result
species10 <- rl_sp(page = 10, key = iucn_token)
species10_df <- species10$result
species11 <- rl_sp(page = 11, key = iucn_token)
species11_df <- species11$result
species12 <- rl_sp(page = 12, key = iucn_token)
species12_df <- species12$result
species13 <- rl_sp(page = 13, key = iucn_token)
species13_df <- species13$result
species14 <- rl_sp(page = 14, key = iucn_token)
species14_df <- species14$result
species15 <- rl_sp(page = 15, key = iucn_token)
species15_df <- species15$result

all_iucn_species <- bind_rows(species0_df, species1_df, species2_df, species3_df, 
                              species4_df, species5_df, species6_df, species7_df,
                              species8_df, species9_df, species10_df, species11_df,
                              species12_df, species13_df, species14_df, species15_df) %>% 
  select(c("scientific_name", "category", "main_common_name")) %>% 
  rename(genus_species = scientific_name)

### ------------------------ Combine & Clean ------------------------  ###

fish_status <- left_join(fish_chars, all_iucn_species,
                            by = "genus_species") # Duplicate rows introduced

# Identify which rows are here multiple times
status_unique <- as.data.frame(table(fish_status$spec_code)) %>% 
  setNames(c("spec_code", "freq")) %>% 
  filter(!freq != 1) # remove rows w freq > 1
#view(status_unique)

# Recombine with status df
fish_status$spec_code <- as.factor(fish_status$spec_code)
fish_status <- left_join(status_unique, fish_status, 
                            by = "spec_code")
# Drop all rows with na values of interest
status_drop_na <- fish_status %>% 
  filter(!category == "NA") %>% 
  filter(!category == "DD") %>% 
  filter(!length_cm == "NA") %>% 
  filter(!coral_reefs == "NA")

# Make a binary column with 1 as some level of concern and 0 as least concern
tidy_fish_data <- status_drop_na %>% 
  mutate(is_of_concern = case_when(category == "CR" | 
                                     category == "EN" |
                                     category == "VU" ~ 1,
                                   category == "LR/nt" |
                                     category == "NT" |
                                     category == "LC" ~ 0)) %>% 
  mutate(coral_reefs = coral_reefs * - 1) %>% 
  mutate(reef_associated = case_when(coral_reefs == 1 ~ "yes",
                                     coral_reefs == 0 ~ "no")) %>% 
  mutate(is_endemic = case_when(status == "endemic" ~ "yes",
                                status == "native" |
                                  status == "introduced" ~ "no"))

### ------------------------ Stats Analysis ------------------------  ###

# Here I look at each piece individually, then combine them together later

# Graph length vs is of concern
gg_len <- ggplot(data = tidy_fish_data, aes(x = length_cm, y = is_of_concern)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Species average length", y = "Listed as a concern") +
  theme_minimal()
gg_len

# Log regression length
mod_length <- glm(is_of_concern ~ length_cm, 
                  data = tidy_fish_data, 
                  family = "binomial")
summary(mod_length)

# Plot with regression
gg_len +
  geom_smooth(method = "glm", 
              se = FALSE, color = "#545454", 
              method.args = list(family = "binomial"))

# Plot reef associated vs is of concern
gg_reef <- ggplot(data = tidy_fish_data, aes(x = reef_associated, y = is_of_concern)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Reef Associated", y = "Listed as a concern") +
  theme_minimal()
gg_reef

# Log regression reefs
mod_reef <- glm(is_of_concern ~ reef_associated, 
                data = tidy_fish_data, 
                family = "binomial")
summary(mod_reef)

# Plot endemism vs is of concern
gg_status <- ggplot(data = tidy_fish_data, aes(x = is_endemic, y = is_of_concern)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Endemic", y = "Listed as a concern") +
  theme_minimal()
gg_status

# Log regression of endemism
mod_status <- glm(is_of_concern ~ is_endemic, 
                  data = tidy_fish_data, 
                  family = "binomial")
summary(mod_status)

# I don't think these last two are correct... ask!!!

# Combining it all together
mod <- glm(is_of_concern ~ length_cm + reef_associated + is_endemic,
           data = tidy_fish_data,
           family = "binomial")
mod

b1 <- mod$coefficients[1]
b2 <- mod$coefficients[2]
b3 <- mod$coefficients[3]
b4 <- mod$coefficients[4]

