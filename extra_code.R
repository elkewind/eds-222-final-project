# Plot reef associated vs is of concern 
gg_reef <- ggplot(data = tidy_fish_data, aes(x = reef_associated, y = is_of_concern)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.8, col = "#38b6ba") +
  labs(x = "Reef Associated", y = "Listed as a concern") +
  theme_minimal()
gg_reef

# Log regression reefs -- does this even make sense to do?
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

# Log regression of endemism -- does this even make sense to do?
mod_status <- glm(is_of_concern ~ is_endemic, 
                  data = tidy_fish_data, 
                  family = "binomial")
summary(mod_status)

# I don't think these last two are correct... ask!!!

# Combining it all together (even though)
mod <- glm(is_of_concern ~ length_cm + reef_associated + is_endemic,
           data = tidy_fish_data,
           family = "binomial")
mod

# Pull out all coefficients
b0 <- mod$coefficients[1] #Intercept
b1 <- mod$coefficients[2] #Length
b2 <- mod$coefficients[3] #Reef Associated
b3 <- mod$coefficients[4] #Endemic

# Run some test probabilities 
equ <- b0 * b1 * 700  * b2 * b3
p_700_1_1 <- (exp(equ)) / (1 + exp(equ))

equ20 <- b0 * b1 * 20 * b2 * b3
p_20_1_1 <- (exp(equ20)) / (1 + exp(equ20))

equ450 <- b0 * b1 * 450 * b2 * b3
p_450_1_1 <- (exp(equ450)) / (1 + exp(equ450))

# Write a function for testing probabilities
threat_prob <- function(b0, b1, b2, b3, len, reef, end) {
  equ <- b0 * b1 * len * b2 * reef * b3 * end
  prob <- (exp(equ)) / (1 + exp(equ))
  print(prob)
}
# Test
threat_prob(b0, b1, b2, b3, 700, 1, 1)
threat_prob(b0, b1, b2, b3, 20, 1, 1)
threat_prob(b0, b1, b2, b3, 450, 1, 1)

# Attempt some t-tests???
reef_test <- t.test(is_of_concern ~ reef_associated, data = tidy_fish_data)
end_test <- t.test(is_of_concern ~ is_endemic, data = tidy_fish_data)
