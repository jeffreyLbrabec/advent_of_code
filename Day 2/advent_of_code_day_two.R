library(tidyverse)
library(here)

input <- read_delim(here("Day 2/input.txt"),
                    delim = ":", 
                    col_names = FALSE) %>% 
  rename(rules = X1, pword = X2) %>% 
  mutate(pword = str_trim(pword))

#Day 2 part 1: Find the number of passwords that are valid

split_cols <- input %>% 
  separate(col = rules, into = c("nums", "lets"), sep = "\ ") %>% 
  separate(col = nums, into = c("lower", "upper"), sep = "-") %>% 
  mutate(across(ends_with("er"), as.numeric))

validity_check <- split_cols %>% 
  rowwise() %>% 
  mutate(
    valid_pword = ifelse(lower <= str_count(pword, lets) & str_count(pword, lets) <= upper, TRUE, FALSE)
  )

solution_one <- sum(validity_check$valid_pword)
solution_one

#Day 2 part 2: Find the number of valid passwords based on updated rule

validity_check_two <- split_cols %>% 
  rowwise() %>% 
  mutate(
    pword_and_cond_check = ifelse(str_sub(pword, start = lower, end = lower) == lets &
                           str_sub(pword, start = upper, end = upper) == lets, TRUE, FALSE)
  ) %>% 
  mutate(
    pword_or_cond_check = ifelse(str_sub(pword, start = lower, end = lower) == lets |
                                   str_sub(pword, start = upper, end  = upper) == lets, TRUE, FALSE)
  ) %>% 
  mutate(
    valid_pword = ifelse(pword_or_cond_check == TRUE & pword_and_cond_check == FALSE, TRUE, FALSE)
  )

solution_two <- sum(validity_check_two$valid_pword)
solution_two

#drobs solution

split_cols %>% 
  rowwise() %>% 
  mutate(
    count = (str_sub(pword, lower, lower) == lets) +
      (str_sub(pword, upper, upper) ==lets)
  ) %>% 
  filter(count == 1)
