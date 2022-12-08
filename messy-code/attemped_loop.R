my_list <- seq(from = 0, to = 1, by = 1)
for (i in my_list) {
  species = rl_sp(page = i, key = iucn_token)
}

for (i in my_list) {
  new_val = i + 2
  print(new_val)
}



for (i in my_list) {
  assign(paste0("species", i)) <- rl_sp(page = i, key = iucn_token)
  #paste0("species", i, "_df") <- species[i]$result
}
assign(paste0("model", run), c(run, 4, 74, 2))

my_vec <- c(0:14)
birddog_sum <- function(bird, dog) {
  pets = bird + dog
  return(pets)
}

species_read_in <- function(number, key)
  species0 = rl_sp(page = 0, key = iucn_token)
species0_df <- species0$result




# Create a list 1-15 since there are 15 tens of thousands of rows 
#of data to read in at 10,000 rows per call
my_list <- list(1, 2, 3, 4) #5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
my_list  
my_assignment <- rl_sp(page = i, key = iucn_token)
my_dfs <-
  
  assign("species_1", my_list[[1]])               # Apply assign function
species_1

my_vec <- c()

for(i in 1:length(my_list)) {                    # assign function within loop
  assign(paste0("species_", i), rl_sp(page = i, key = iucn_token))
  my_vec <- c(my_vec, (paste0("species_", i)))
  #assign(paste0("species_", i, "_df"), paste0("species_", i)[['result']])
}

for (i in my_vec) {
  assign(paste0("species_", i, "_df"), i$result)
}


species0_df <- species0$result