library(tidyverse)
library(Matrix)

vents <- readLines("input") %>%
    tibble(.vent_txt = .) %>%
    transmute(str_match(.vent_txt, '([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)') %>%
                   as_tibble(.name_repair = ~ c("matched", "x1", "y1", "x2", "y2"))) %>%
    select(-matched) %>%
    mutate(across(everything(), as.integer))

dim <- 1 + vents %>%
    pivot_longer(everything()) %>%
    pull(value) %>%
    max

straight_line <- function (dim, x1, y1, x2, y2) {
    x1 <- x1 + 1
    x2 <- x2 + 1
    y1 <- y1 + 1
    y2 <- y2 + 1

    if (x1 == x2) {
        cols <- c(y1:y2)
        sparseMatrix(
            i = rep(x1, length(cols)),
            j = cols,
            x = 1,
            dims = c(dim, dim))
    } else if (y1 == y2) {
        lines <- c(x1:x2)
        sparseMatrix(
            i = lines,
            j = rep(y1, length(lines)),
            x = 1,
            dims = c(dim, dim))
    } else {
        sparseMatrix(c(), c(), dims = c(dim, dim))
    }
}

# Part 1

map <- vents %>%
    rowwise %>%
    group_map(~ straight_line(dim, .$x1, .$y1, .$x2, .$y2)) %>%
    Reduce('+', .)

nnzero(map >= 2)

# Part 2

straight_or_diagonal_line <- function (dim, x1, y1, x2, y2) {
    if (x1 == x2 || y1 == y2) {
        straight_line(dim, x1, y1, x2, y2)
    } else if (abs(x1 - x2) == abs(y1 - y2)) {
        x1 <- x1 + 1
        x2 <- x2 + 1
        y1 <- y1 + 1
        y2 <- y2 + 1
        sparseMatrix(
            c(x1:x2),
            c(y1:y2),
            x = 1,
            dims = c(dim, dim))
    } else {
        sparseMatrix(c(), c(), dims = c(dim, dim))
    }
}

map_diag <- vents %>%
    rowwise %>%
    group_map(~ straight_or_diagonal_line(dim, .$x1, .$y1, .$x2, .$y2)) %>%
    Reduce('+', .)

nnzero(map_diag >= 2)
