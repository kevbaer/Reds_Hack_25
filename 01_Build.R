library(tidyverse)

sample_sub <- read_csv("Data/sample_submission.csv")

lahman <- read_csv("Data/lahman_people.csv") |>
  mutate(debut_year = year(debut))

codebook_savant <- readxl::read_excel("Data/codebook.xlsx", "Baseball Savant")

codebook_lahman <- readxl::read_excel("Data/codebook.xlsx", "Lahman")


savant_23 <- duckplyr::duckplyr_df_from_csv("Data/savant_data_2021_2023.csv") |>
  duckplyr::filter(game_year == 2023) |>
  as_tibble()

savant_22 <- duckplyr::duckplyr_df_from_csv("Data/savant_data_2021_2023.csv") |>
  duckplyr::filter(game_year == 2022) |>
  as_tibble()

savant_21 <- duckplyr::duckplyr_df_from_csv("Data/savant_data_2021_2023.csv") |>
  duckplyr::filter(game_year == 2021) |>
  as_tibble()

batters_23_x <- tibble(PLAYER_ID = unique(savant_23$batter))

pitchers_23_x <- tibble(PLAYER_ID = unique(savant_23$pitcher))

plate_appearances <- savant_23 |>
  group_by(
    batter,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    batter,
  ) |>
  summarize(
    playing_time = n()
  ) |>
  ungroup()

# plate_appearances_average <- plate_appearances |>
#   group_by(batter) |>
#   summarize(
#     playing_time = round(mean(playing_time), 0)
#   ) |>
#   ungroup()

batters_face_average <- savant_23 |>
  group_by(
    pitcher,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    pitcher,
  ) |>
  summarize(playing_time = n()) |>
  ungroup()



batters_23 <- batters_23_x |>
  left_join(
    plate_appearances |>
      rename(PLAYER_ID = batter, ACTUAL_TIME = playing_time),
    by = "PLAYER_ID"
  )



pitchers_23 <- pitchers_23_x |>
  left_join(
    batters_face_average |>
      rename(PLAYER_ID = pitcher, ACTUAL_TIME = playing_time),
    by = "PLAYER_ID"
  )

zz <- sample_sub |>
  mutate(y23 = PLAYER_ID %in% savant_23$batter | PLAYER_ID %in% savant_23$pitcher) |>
  mutate(y22 = PLAYER_ID %in% savant_22$batter | PLAYER_ID %in% savant_22$pitcher) |>
  mutate(y21 = PLAYER_ID %in% savant_21$batter | PLAYER_ID %in% savant_21$pitcher) |>
  rowwise() |>
  mutate(n = sum(c_across(starts_with("y")))) |>
  ungroup() |>
  left_join(
    lahman |>
      select(player_mlb_id, birthYear, birthCountry, weight, height, throws, bats, debut_year),
    by = join_by(PLAYER_ID == player_mlb_id)
  )




# combined_playing_time <- plate_appearances |>
#   rename(
#     PLAYER_ID = batter,
#     PLAYING_TIME = playing_time,
#   ) |>
#   bind_rows(batters_face_average |>
#               rename(PLAYER_ID = pitcher,
#                      PLAYING_TIME = playing_time)) |>
#   group_by(PLAYER_ID) |>
#   summarize(PLAYING_TIME = sum(PLAYING_TIME)) |>
#   ungroup()

#
# sub <-
#   sample_sub |>
#   select(-PLAYING_TIME) |>
#   left_join(combined_playing_time, by = "PLAYER_ID")

# write_csv(sub, file = "submissions/submission_2-8-25_TEST.csv")
