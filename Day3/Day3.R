library(tidyverse)
library(stringr)

bits <- read.table("input",
                   header = FALSE,
                   col.names = c("bits"),
                   colClasses = c("character"))

# Part 1: gamma and epsilon (most frequent resp. least frequent bits
# in report)

freqs <- function(bits_v) {
    bit.width <- nchar(bits_v) %>% unique
    stopifnot(length(bit.width) == 1)

    str_split_fixed(bits_v, pattern = "", n = bit.width) %>%
    as_tibble(.name_repair = function(cols) { c((length(cols) - 1):0) %>% as.character }) %>%
    pivot_longer(everything(), names_to = "weight") %>%
    group_by(weight) %>%
    summarize(.groups = "drop",
              value %>% table %>% as_tibble) %>%
    rename(bit = ".")
}

greek <- bits$bits %>%
    freqs %>%
    group_by(weight) %>%
    group_modify(~ .x %>%
                     arrange(desc(n)) %>%
                     add_column(greek = c("gamma", "epsilon"))) %>%
    ungroup

answers <- greek %>%
    mutate(value = 2^as.integer(weight) * as.integer(bit)) %>%
    group_by(greek) %>%
    summarise(value = sum(value)) %>%
    pivot_wider(names_from = "greek", values_from = "value")

answers$epsilon * answers$gamma

# Part 2: oxygen generator rating and CO2 scrubber rating

rating <- function (bits_v, gas) {
    bit.width <- nchar(bits_v) %>% unique
    stopifnot(length(bit.width) == 1)

    for(w in c((bit.width - 1):0)) {
        ## print(bits_v)  # XXX

        filt <- freqs(bits_v) %>%
            filter(weight == w) %>%
            arrange(desc(n), desc(as.integer(bit)))

        ## print(filt)  # XXX

        if (gas == "oxygen") {
            filt <- head(filt, 1)
        } else if (gas == "CO2") {
            filt <- tail(filt, 1)
        } else {
            stop(str_glue("Bad gas: ", gas))
        }

        ## print(filt)  # XXX
        keep <- tibble(bits = bits_v) %>%
            mutate(str_match(bits, str_glue('^(', strrep('.', bit.width - 1 - w), ')(.)(.*)$')) %>%
                   as_tibble(.name_repair = ~ c("matched", "before", "bit", "after"))) %>%
            filter(bit == filt$bit)

        ## print(keep) # XXX

        bits_v <- keep %>% pull(bits)

        if (length(bits_v) == 1) return(bits_v %>% strtoi(2))
    }
}

rating(bits$bits, "oxygen") * rating(bits$bits, "CO2")
