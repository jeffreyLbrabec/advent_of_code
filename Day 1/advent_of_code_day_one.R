library(tidyverse)
library(here)

input <- as.matrix(read_table(here("Day 1/input.txt"), col_names = FALSE))

#Day 1 part 1: Find the two numbers that sum to 2020 and find the product
for(i in 1:length(input)) {
  for(j in 1:length(input)) {
    if((input[i] + input [j]) == 2020) {
      solution <- input[i] * input[j]
    }
  }
}
solution

#Day 1 part 2: Find the three numbers that sum to 2020 and find the product
for(i in 1:length(input)) {
  for(j in 1:length(input)) {
    for(k in 1:length(input)) {
      if((input[i] + input [j] + input[k]) == 2020) {
        solution_two <- input[i] * input[j] * input[k]
      }
    }
  }
}
solution_two