library(tibble)

random_string_column <- function(n) {
    stringi::stri_rand_strings(n = n, length = 8)
}

random_data_frame <- function(n) tibble(
    col1 = random_string_column(n),
    col2 = random_string_column(n)
)

data <- random_data_frame(10^7)

time <- function(...) {
    time_measurement <- system.time(eval(...))
    time_measurement[["user.self"]]
}

benchmark <- function(..., n = 100) {
    times <- replicate(n, ...)
    c(min = min(times), max = max(times), mean = mean(times))
}

library(dplyr)

benchmark({
    key_to_lookup <- select_random_key()
    time(data %>% filter(col1 == key_to_lookup))
})

library(data.table)

data_table <- data.table(data)

benchmark({
    key_to_lookup <- select_random_key()
    time(data_table[col1 == key_to_lookup])
})

time(setkey(data_table, col1))

benchmark({
    key_to_lookup <- select_random_key()
    time(data_table[.(key_to_lookup), nomatch = 0L])
})