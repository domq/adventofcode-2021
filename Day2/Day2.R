library(tidyverse)

moves <- read.table("input",
              header = FALSE,
              col.names = c("direction", "amount")) %>%
    tibble

# Part 1
totals <- moves %>%
    group_by(direction) %>%
    summarize(amount = sum(amount)) %>%
    pivot_wider(names_from = direction, values_from = amount)

x <- totals$forward
y <- totals$down - totals$up
x * y

# Part 2
course <- moves %>%
    mutate(aim = cumsum(case_when(direction == "down" ~ amount,
                                  direction == "up"   ~ -amount,
                                  TRUE                ~ 0L))) %>%
    filter(direction == "forward") %>%
    mutate(x = cumsum(amount),
           y = cumsum(amount * aim))

final_pos <- course %>% tail(n = 1)
final_pos$x * final_pos$y
