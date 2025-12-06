library(reader)
library(dplyr)

state_election_df <- read.csv("/Users/amalabdi/Downloads/correlates2-6.csv", header = TRUE)

names(state_election_df)


    
gov_df <- state_election_df %>%
      mutate(
        govparty = coalesce(
          govparty_c,
          govparty_b_2,
          govparty_b,
          govparty_a
        )
      ) %>%
      filter(year >= 1975) %>%
      select(state, year, govparty)


gov_df <-  gov_df |>
  mutate(
    govparty_label = case_when(
      govparty == 0 ~ "Republican",
      govparty == 1 ~ "Democratic",
      govparty == .5 ~ "Independent/Third Party",
      TRUE ~ NA_character_
    )
  )

View(gov_df)
