library(tidyverse)

testdata <- c(16,1,2,0,4,2,7,1,2,14)
input <- (readLines("input") %>% str_split(","))[[1]] %>% as.double

optimize <- function(crabs_v, cost_fn) {
    tibble(goal = c(min(crabs_v):max(crabs_v))) %>%
        mutate(cost = map_dbl(goal, ~ cost_fn(crabs_v, .x))) %>%
        filter(cost == min(cost)) %>%
        head(1)
}

# Part 1
optimize(input, function(crabs_v, x) { sum(abs(crabs_v - x)) })$cost

# Part 2
optimize(input, function(crabs_v, x) {
    distances <- abs(crabs_v - x)
    sum((distances * (distances + 1)) / 2)
})$cost
