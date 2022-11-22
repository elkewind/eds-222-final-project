library(rfishbase)
library(tidyverse)
library(janitor)

# Load all species
species <- fb_tbl("species") %>% 
  janitor::clean_names()

# Load all species by country
country <- fb_tbl("country") %>% 
  janitor::clean_names()

# Find all species that might live in hawaii
hi_fish <- country %>% 
  filter(c_code == "840B")

# Join hi_fish with species info
hi_fish <- left_join(hi_fish, species, by = "spec_code")

# Concatenate Genus and Species
hi_fish$genus_species <- paste(hi_fish$genus, hi_fish$species)
hi_fish <- hi_fish %>% relocate(genus_species, .after = spec_code) %>% 
  relocate(length, weight, .after = genus_species)
hi_species_list <- as.character(hi_fish$genus_species) # Isolate list of species

# Check out what is available in the various tables
tables <- docs()
ecology <- fb_tbl("ecology")

# Grab the ecological traits I'm looking for
hi_ecol <- ecology(species_list = hi_species_list, 
                   fields = c("SpecCode", "CoralReefs", "FeedingType", "Schooling")) %>% 
  clean_names()

# Combine datasets then clean
hi_fish_chars <- left_join(hi_fish, hi_ecol, by = "spec_code") %>% 
  select(c("spec_code", "genus_species", "length", "weight", 
           "status", "current_presence", "genus", "species", "importance.y", 
           "price_categ", "coral_reefs", "feeding_type", "schooling")) %>% 
  filter(current_presence == "present")




