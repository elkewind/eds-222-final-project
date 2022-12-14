#| include: false
### ------------------- EDS 222 Final Project ------------------- ###
### Identifying key traits in Hawaiian fish to predict risk of extinction ###
# Load libraries
library(rfishbase)
library(tidyverse)
library(janitor)
library(jsonlite)
library(rredlist)
library(broom)
library(sjPlot)
# Set data export location
datadir <- "/Users/elkewindschitl/Documents/MEDS/eds-222/final-proj/data"
### ------------------------ Stats Analysis ------------------------  ###
tidy_fish_data <- read_csv(file.path(datadir, "hi_tidy_fish_data.csv"))
hi_fish_status <- read_csv(file.path(datadir, "hi_fish_status.csv"))

tidy_fish_data <- tidy_fish_data %>% 
  mutate(is_threatened = case_when(is_of_concern == 1 ~ "yes",
                                   is_of_concern == 0 ~ "no"))
# Here I look at each piece individually, then combine them together later
rm_len_na <- tidy_fish_data %>% 
  filter(!length_cm == "NA")
# Graph length vs is of concern
gg_len <- ggplot(data = rm_len_na, aes(x = length_cm, y = is_of_concern)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Species average length", y = "Listed as a concern") +
  theme_minimal() +
  labs(x = "Species length (cm)", y = "Listed as threatened", title = "Fig 2. Probability of being threatened by species length") +
  theme(panel.background = element_rect(fill = "#f7f2e6"),
        plot.background = element_rect(fill = "#f7f2e6"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = '#c9c9c9'),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
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
# Run this again, but remove large values to evaluate robustness
rm_outliers <- rm_len_na %>% 
  filter(length_cm <= 1000)
# Graph length vs is of concern
gg_rm_out <- ggplot(data = rm_outliers, aes(x = length_cm, y = is_of_concern)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Species length (cm)", y = "Listed as threatened", title = "Fig 3. Probability of being threatened by species length \n (excluding outliers)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#f7f2e6"),
        plot.background = element_rect(fill = "#f7f2e6"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = '#c9c9c9'),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
gg_rm_out
# Log regression length
mod_rm_out <- glm(is_of_concern ~ length_cm, 
                  data = rm_outliers, 
                  family = "binomial")
summary(mod_rm_out)
# Plot with regression
len_rm_out_plot <- gg_rm_out +
  geom_smooth(method = "glm", 
              se = FALSE, color = "#545454", 
              method.args = list(family = "binomial"))
len_rm_out_plot
# Make bins
len_breaks <- rm_len_na %>%
  pull(length_cm) %>%
  quantile(probs = 0:10/10)

len_binned_space <- len_data_space + 
  stat_summary_bin(
    fun = "mean", color = "red", 
    geom = "line", breaks = len_breaks
  )
len_binned_space
# Compute fitted probabilities, then graph
length_plus <- mod_length %>%
  augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)
ggplot(length_plus, aes(x = length_cm, y = y_hat)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Probabilities of being threatened", 
                     limits = c(0,1))
# Compute odds scale and graph it
length_plus <- length_plus %>% 
  mutate(odds_hat = y_hat / (1 - y_hat)) %>% 
  filter(length_cm <= 1000) # remove outliers for graphing
len_odds_plot <- ggplot(length_plus, aes(x = length_cm, y = odds_hat)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Odds of being threatened") +
  labs(x = "Species length (cm)", title = "Fig 4. Odds of being threatened by species length") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#f7f2e6"),
        plot.background = element_rect(fill = "#f7f2e6"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = '#c9c9c9'),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
len_odds_plot
# Pull B1
len_b1 <- mod_length$coefficients[2]
len_odds_ratio <- exp(len_b1)
print(paste0("The model suggests that each additional cm in length is associated with a ",
             round(len_odds_ratio, 1), "% increase in the odds of being threatened"))
# Compute log-odds and graph it
length_plus <- length_plus %>% 
  mutate(log_odds_hat = log(odds_hat))
ggplot(length_plus, aes(x = length_cm, y = log_odds_hat)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Log(odds) of being threatened")
# Create confusion matrix to see how well the model performed
length_plus <- augment(mod_length, type.predict = "response") %>%
  mutate(threatened_hat = round(.fitted)) %>%
  select(is_of_concern, length_cm, .fitted, threatened_hat)
l_tab <- length_plus %>%
  select(is_of_concern, threatened_hat) %>%
  table()
l_tab
acc <- (l_tab[1,1] + l_tab[2,2]) / nrow(length_plus) * 100
print(paste0("The accuracy of this model was ", round(acc), "%")) 
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
# Attempt some t-tests???
t.test(is_of_concern ~ reef_associated, data = rm_ra_na)
t.test(is_of_concern ~ is_endemic, data = tidy_fish_data)

### ------------------------ Predictions ------------------------  ###

# Filter for all columns of data deficient fish in Hawaii
hi_fish_no_rank <- hi_fish_status %>% 
  filter(is.na(category) | category == "DD")
# Tidy data
tidy_no_rank <- hi_fish_no_rank %>% 
  mutate(coral_reefs = coral_reefs * - 1) %>% 
  mutate(reef_associated = case_when(coral_reefs == 1 ~ "yes",
                                     coral_reefs == 0 ~ "no")) %>% 
  mutate(is_endemic = case_when(status == "endemic" ~ 1,
                                status == "native" |
                                  status == "introduced" ~ 0))
# Create loop to populate prob in every row
for (i in seq_along(tidy_no_rank$genus_species)) {
  tidy_no_rank$y_hat[i] <- threat_prob(b0, b1, b2, b3, 
                                       tidy_no_rank$length_cm[i],
                                       tidy_no_rank$coral_reefs[i],
                                       tidy_no_rank$is_endemic[i])
}
# Arrange by probability 
tidy_pred_rank <- tidy_no_rank %>% arrange(desc(y_hat)) %>% 
  mutate(endemic = case_when(status == "endemic" ~ "yes",
                             status == "native" |
                               status == "introduced" ~ "no"))
