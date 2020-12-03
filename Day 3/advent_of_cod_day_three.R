library(tidyverse)
library(here)

input <- read_table(here("Day 3/input.txt"),
                    col_names = FALSE)

input_map <- str_split_fixed(input$X1, "", n = Inf)

full_map <- do.call("cbind", replicate(32, input_mat, simplify = FALSE))

#Day 3 part 1: How many trees would I hit if I went 3 across 1 down till i hit the bottom of the map?
map_to_plot <- full_map

ri <- 1
ci <- 1

for(i in 1:(nrow(map_to_plot)-1)) {
  
  if(map_to_plot[ri + 1, ci + 3] == ".") {
    map_to_plot[ri + 1, ci + 3] <- "O"
  }else{
    map_to_plot[ri + 1, ci + 3] <- "X"
  }
  
  ri <- ri + 1
  ci <- ci + 3
  
}

solution1 <- sum(map_to_plot == "X")

#Day 3 part 2: determine number of trees you would encounter for each slope and multiply together
# r1, d1
# r3, d1
# r5, d1
# r7, d1
# r1, d2

#Let's package the above code into a function:

tree_hitter <- function(tree_map, right = 3, down = 1, iter = (nrow(tree_map) - 1)) {
  
  ri <- 1
  ci <- 1
  
  for(i in 1:iter) {
    
    if(tree_map[ri + down, ci + right] == ".") {
      tree_map[ri + down, ci + right] <- "O"
    }else{
      tree_map[ri + down, ci + right] <- "X"
    }
    
    ri <- ri + down
    ci <- ci + right
    
  }
  
  trees <- sum(tree_map == "X")
  return(trees)
}

# r1, d1
map_to_plot_r1d1 <- full_map
first <- tree_hitter(map_to_plot_r1d1, right = 1, down = 1)

# r3, d1
map_to_plot_r3d1 <- full_map
second <- tree_hitter(map_to_plot_r3d1, right = 3, down = 1)

# r5, d1
map_to_plot_r5d1 <- do.call("cbind", replicate(53, input_mat, simplify = FALSE))
third <- tree_hitter(map_to_plot_r5d1, right = 5, down = 1)

# r7, d1
map_to_plot_r7d1 <- do.call("cbind", replicate(73, input_mat, simplify = FALSE))
fourth <- tree_hitter(map_to_plot_r7d1, right = 7, down = 1)

# r1, d2
map_to_plot_r1d2 <- do.call("cbind", replicate(10, input_mat, simplify = FALSE))
fifth <- tree_hitter(map_to_plot_r1d2, right = 1, down = 2, iter = 161)

solution2 <- first * second * third * fourth * fifth
