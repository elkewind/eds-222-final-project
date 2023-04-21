### ------------------- EDS 222 Final Project ------------------- ###
### This is the original code for the following blog post (completed in early December 2022).
### Feel free to check it out:
### https://elkewind.github.io/posts/2022-12-02-hawaiian-fish-analysis/
### Identifying key traits in Hawaiian fish to predict risk of extinction ###

# Load libraries
library(rfishbase)
library(tidyverse)
library(janitor)
library(jsonlite)
library(rredlist)
library(broom)

# Set data export location
dataexp <- "/Users/elkewindschitl/Documents/MEDS/eds-222/final-proj/data"

### -------------------------- FishBase --------------------------  ###

# Load all species in FishBase
species <- fb_tbl("species") %>% 
  janitor::clean_names()

# Load all species by country
country <- fb_tbl("country") %>% 
  janitor::clean_names()

# Find all species that might live in Hawaii
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
com_names <- fb_tbl("comnames")

# Grab the ecological traits I'm looking for
hi_ecol <- ecology(species_list = hi_species_list, 
                   fields = c("SpecCode", "CoralReefs", "FeedingType", "Schooling")) %>% 
  clean_names() # Theres duplicates here but I remove them later
hi_com_names <- common_names(species_list = hi_species_list,
                             fields = c("SpecCode", "ComName")) %>% 
  clean_names()

# Combine data sets then clean
hi_fish_chars <- left_join(hi_fish, hi_ecol, by = "spec_code") %>% 
  select(c("spec_code", "genus_species", "length", "l_type_max_m", "weight", 
           "status", "current_presence", "genus", "species", "importance.y", 
           "price_categ", "coral_reefs", "feeding_type", "schooling")) %>% 
  filter(current_presence == "present") %>% # Data frame full of fish characteristics
  rename(length_cm = length)

### --------------------------  IUCN --------------------------  ###

# Identify token for accessing IUCN API
iucn_token <- Sys.getenv("IUCN_KEY")

# Import all species on IUCN Redlist
for (i in 0:15) {
  # Get data from API and assign to variable with a name
  assign(paste0("species", i), rl_sp(page = i, key = iucn_token))
  # Get the result data frame from the variable and assign to a new variable with a name
  assign(paste0("species", i, "_df"), get(paste0("species", i))$result)
}


species0 <- rl_sp(page = 0, key = iucn_token)
species0_df <- species0$result

all_iucn_species <- bind_rows(species0_df, species1_df, species2_df, species3_df, 
                              species4_df, species5_df, species6_df, species7_df,
                              species8_df, species9_df, species10_df, species11_df,
                              species12_df, species13_df, species14_df, species15_df)

# Save this as a csv
#write.csv(all_iucn_species, file.path(dataexp, "all_iucn_species.csv"))

all_iucn_species <- all_iucn_species %>%  select(c("scientific_name", "category", "main_common_name")) %>% 
  rename(genus_species = scientific_name)

### ------------------------ Combine & Clean ------------------------  ###

hi_fish_status <- left_join(hi_fish_chars, all_iucn_species,
                            by = "genus_species") # Duplicate rows introduced

# Identify which rows are here multiple times
status_unique <- as.data.frame(table(hi_fish_status$spec_code)) %>% 
  setNames(c("spec_code", "freq")) %>% 
  filter(!freq != 1) # remove rows w freq > 1
#view(status_unique)

# Recombine with status df
hi_fish_status$spec_code <- as.factor(hi_fish_status$spec_code)
hi_fish_status <- left_join(status_unique, hi_fish_status, 
                            by = "spec_code")
# Drop all rows with na values of interest
hi_status_drop_na <- hi_fish_status %>% 
  filter(!category == "NA") %>% 
  filter(!category == "DD")

# Make a binary column with 1 as some level of concern and 0 as least concern
tidy_fish_data <- hi_status_drop_na %>% 
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

#write.csv(tidy_fish_data, file.path(dataexp, "hi_tidy_fish_data.csv"))
#write.csv(hi_fish_status, file.path(dataexp, "hi_fish_status.csv"))

### ------------------------ Stats Analysis ------------------------  ###

# Here I model each piece individually, then combine them together later

rm_len_na <- tidy_fish_data %>% 
  filter(!length_cm == "NA")

# Graph length vs is of concern
gg_len <- ggplot(data = rm_len_na, aes(x = length_cm, y = is_of_concern)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Species average length", y = "Listed as a concern") +
  theme_minimal()
gg_len

# Log regression length
mod_length <- glm(is_of_concern ~ length_cm, 
                  data = rm_len_na, 
                  family = "binomial")
summary(mod_length)

# Plot with regression
len_data_space <- gg_len +
  geom_smooth(method = "glm", 
              se = FALSE, color = "#545454", 
              method.args = list(family = "binomial"))
len_data_space


# Compute fitter probabilities, then graph
length_plus <- mod_length %>%
  augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)
ggplot(length_plus, aes(x = length_cm, y = y_hat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Probabilities of being threatened", 
                     limits = c(0,1)) +
  theme_minimal()

# Compute odds scale and graph it
length_plus <- length_plus %>% 
  mutate(odds_hat = y_hat / (1 - y_hat)) %>% 
  filter(length_cm <= 1000) # remove outliers for graphing
ggplot(length_plus, aes(x = length_cm, y = odds_hat)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Odds of being threatened") +
  theme_minimal()

# Pull B1
len_b1 <- mod_length$coefficients[2]
len_odds_ratio <- exp(len_b1)
print(paste0("The model suggests that each additional cm in length is associated with a ",
             round(len_odds_ratio, 1), "% increase in the odds of being threatened"))

# Compute log-odds and graph it
length_plus <- length_plus %>% 
  mutate(log_odds_hat = log(odds_hat))
ggplot(length_plus, aes(x = length (cm), y = log_odds_hat)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Log(odds) of being threatened") +
  theme_minimal()

# Create confusion matrix to see how well the model performed
length_plus <- augment(mod_length, type.predict = "response") %>%
  mutate(threatened_hat = round(.fitted)) %>%
  select(is_of_concern, length_cm, .fitted, threatened_hat)
l_tab <- length_plus %>%
  select(is_of_concern, threatened_hat) %>%
  table()
l_tab

acc <- (l_tab[1,1] + l_tab[2,2]) / nrow(length_plus) * 100
print(paste0("The accuracy of this model was ", round(acc), "%. However, it seems to be more accurate in predicting species that are not actually of concern. Species that are actually threatened have poorer prediction rates.")) 





# Add reef association to the model
tidy_no_na <- rm_len_na %>% 
  filter(!coral_reefs == "NA")

len_reef_mod <- glm(is_of_concern ~ length_cm + reef_associated,
                    data = tidy_no_na,
                    family = "binomial")
len_reef_mod
print(paste0("Fish that are reef associated see their odds of being threatened decrease by a factor of ", -round(len_reef_mod$coefficients[3], 2), " after controlling for length"))

# Create confusion matrix to see how well the model performed
length_reef_plus <- augment(len_reef_mod, type.predict = "response") %>%
  mutate(threatened_hat = round(.fitted)) %>%
  select(is_of_concern, length_cm, reef_associated, .fitted, threatened_hat)
l_r_tab <- length_reef_plus %>%
  select(is_of_concern, threatened_hat) %>%
  table()
l_r_tab # Adding reef did nothing





# Add endemism to the original length model
len_end_mod <- glm(is_of_concern ~ length_cm + is_endemic,
                   data = rm_len_na,
                   family = "binomial")
summary(len_end_mod)
print(paste0("Fish that are endemic see their odds of being threatened increase by a factor of ", round(len_end_mod$coefficients[3], 2), " after controlling for length"))

# Create confusion matrix to see how well the model performed
length_end_plus <- augment(len_end_mod, type.predict = "response") %>%
  mutate(threatened_hat = round(.fitted)) %>%
  select(is_of_concern, length_cm, is_endemic, .fitted, threatened_hat)
l_e_tab <- length_end_plus %>%
  select(is_of_concern, threatened_hat) %>%
  table()
l_e_tab # Adding endemism did nothing








rm_ra_na <- tidy_fish_data %>% 
  filter(!coral_reefs == "NA")

# Plot reef associated vs is of concern 
gg_reef <- ggplot(data = rm_ra_na, aes(x = reef_associated, y = is_of_concern)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Reef Associated", y = "Listed as a concern") +
  theme_minimal()
gg_reef

# Log regression reefs -- does this even make sense to do?
mod_reef <- glm(is_of_concern ~ reef_associated, 
                data = rm_ra_na, 
                family = "binomial")
summary(mod_reef)

# Plot endemism vs is of concern
gg_status <- ggplot(data = tidy_fish_data, aes(x = is_endemic, y = is_of_concern)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Endemic", y = "Listed as a concern") +
  theme_minimal()
gg_status

# Log regression of endemism -- does this even make sense to do?
mod_status <- glm(is_of_concern ~ is_endemic, 
                  data = tidy_fish_data, 
                  family = "binomial")
summary(mod_status)

# I don't think these last two are correct... ask!!!

# Combining it all together (even though)
mod <- glm(is_of_concern ~ length_cm + reef_associated + is_endemic,
           data = tidy_no_na,
           family = "binomial")
summary(mod)

# Pull out all coefficients
b0 <- mod$coefficients[1] #Intercept
b1 <- mod$coefficients[2] #Length
b2 <- mod$coefficients[3] #Reef Associated
b3 <- mod$coefficients[4] #Endemic

# Run some test probabilities 
equ <- b0 + b1 * 700  + b2 + b3
p_700_1_1 <- (exp(equ)) / (1 + exp(equ))

equ20 <- b0 + b1 * 20  + b2 + b3
p_20_1_1 <- (exp(equ20)) / (1 + exp(equ20))

equ450 <- b0 + b1 * 450  + b2 + b3
p_450_1_1 <- (exp(equ450)) / (1 + exp(equ450))

# Write a function for testing probabilities
threat_prob <- function(b0, b1, b2, b3, len, reef, end) {
  equ <- b0 + b1 * len + b2 * reef + b3 * end
  prob <- (exp(equ)) / (1 + exp(equ))
  print(prob)
}
# Test
threat_prob(b0, b1, b2, b3, 700, 1, 1)
threat_prob(b0, b1, b2, b3, 20, 1, 1)
threat_prob(b0, b1, b2, b3, 450, 1, 1)

# Attempt some t-tests
t.test(is_of_concern ~ reef_associated, data = rm_ra_na)
t.test(is_of_concern ~ is_endemic, data = tidy_fish_data)

### ------------------------ Predictions ------------------------  ###

# Make predictions for species not listed on IUCN

hi_fish_no_rank <- hi_fish_status %>% 
  filter(is.na(category) | category == "DD") # data deficient species

tidy_no_rank <- hi_fish_no_rank %>% 
  mutate(coral_reefs = coral_reefs * - 1) %>% 
  mutate(reef_associated = case_when(coral_reefs == 1 ~ "yes",
                                     coral_reefs == 0 ~ "no")) %>% 
  mutate(is_endemic = case_when(status == "endemic" ~ 1,
                                status == "native" |
                                  status == "introduced" ~ 0))

# apply model to species that are data deficient
for (i in seq_along(tidy_no_rank$genus_species)) {
  tidy_no_rank$y_hat[i] <- threat_prob(b0, b1, b2, b3, 
                                       tidy_no_rank$length_cm[i],
                                       tidy_no_rank$coral_reefs[i],
                                       tidy_no_rank$is_endemic[i])
}

tidy_pred_rank <- tidy_no_rank %>% arrange(desc(y_hat))
head(tidy_pred_rank)


