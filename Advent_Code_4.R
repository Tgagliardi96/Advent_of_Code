library(tidyverse)
library(stringr)
library(purrr)

input <- read_lines("C:/Users/Tommaso/OneDrive/ML_proj/Advent_Code_Day_4.txt") %>%
  str_remove_all("Card [ ]+")

input <- str_remove_all(input, ".*:")

win_num <- str_extract(input, ".*\\|")
win_num <- str_extract_all(input, "\\d+")
win_num <- lapply(win_num, as.integer)

card_num <- str_extract(input, "\\|.*")
card_num <- str_extract_all(input, "\\d+")
card_num <- lapply(card_num, as.integer)

winner <- function(x) {
  if(x == 0){
    return(0)
  }
  2 ^ (x - 1)
}

purrr::map2_int(win_num, card_num, ~ winner(sum(.x %in% .y))) |>
  sum()