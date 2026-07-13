# Count unique categories
cars_cleaned %>% summarize(across(where(is.factor) | where(is.character), n_distinct))