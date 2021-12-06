library(tidyverse)
library(bit64)

generation <- function(fish, reset = 6, spawn = 8) {
    dec <- tibble(counter = fish - 1)

    count.new <- dec %>%
        count(counter) %>%
        filter(counter == -1) %>%
        pull(n)

    if (length(count.new) == 0) count.new <- 0

    bind_rows(dec %>% mutate(counter = ifelse(counter < 0, reset, counter)),
              rep(spawn, count.new) %>% tibble(counter = .)) %>%
        pull(counter)
}

stopifnot(generation(c(4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8)) ==
          c(3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8))
stopifnot(generation(c(5,6,5,3,4,5,6,7,7,8)) ==
          c(4,5,4,2,3,4,5,6,6,7))

iterate <- function(initial, count, f) {
    Reduce(
        (function(x, ignore) { f(x) }),
        1:count, init = initial)
}
generations <- function(initial, count) { iterate(initial, count, generation) }

stopifnot(generations(c(3,4,3,1,2), 18) ==
          c(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8))

stopifnot(generations(c(3,4,3,1,2), 80) %>% length == 5934)

# Part 1

initial <- (readLines("input") %>% str_split(","))[[1]] %>% as.double
generations(initial, 80) %>% length

# Part 2

fish_hist <- function(state) {
    tibble(counter = state) %>% count(counter) %>%
        mutate(n = as.integer64(n))
}

generation_summarized <- function(fish_hist, reset = 6L, spawn = 8L) {
    dec <- fish_hist %>% mutate(counter = counter - 1)
    count.new <- dec %>% filter(counter == -1) %>% pull(n)
    if (length(count.new) == 0) count.new <- 0
    bind_rows(dec %>% mutate(counter = ifelse(counter < 0, reset, counter)),
              tribble(~counter, ~n, spawn, as.integer64(count.new))) %>%
        group_by(counter) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        arrange(counter)
}

generations_summarized <- function(initial, count) {
    iterate(initial, count, generation_summarized)
}

stopifnot(generations_summarized(fish_hist(c(3,4,3,1,2)), 18) ==
          fish_hist(c(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)))

stopifnot(generations_summarized(fish_hist(c(3,4,3,1,2)), 256) %>% pull(n) %>% sum ==
          26984457539)

generations_summarized(fish_hist(initial), 256) %>% pull(n) %>% sum
