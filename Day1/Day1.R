library(tidyverse)

# part 1
t <- read.csv("input",
              header = FALSE,
              col.names = c("depth")) %>%
    tibble

t %>%
    mutate(prev = lag(depth)) %>%
    filter(depth > prev) %>%
    nrow

# part 2
t %>%
    mutate(moving = depth + lag(depth, 1) + lag(depth, 2)) %>%
    mutate(prev = lag(moving)) %>%
    filter(moving > prev) %>%
    nrow
