source("01_Build.R")

plate_22 <- savant_22 |>
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

mound_22 <- savant_22 |>
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

plate_21 <- savant_21 |>
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

mound_21 <- savant_21 |>
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


batters_23_enhanced <- batters_23 |>
  left_join(
    plate_22 |>
      rename(PLAYER_ID = batter, PENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  left_join(
    plate_21 |>
      rename(PLAYER_ID = batter, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  inner_join(
    lahman |>
      select(player_mlb_id, birthYear, birthCountry, weight, height, bats, debut_year),
    by = join_by(PLAYER_ID == player_mlb_id)
  ) |>
  mutate(birthCountry = as_factor(birthCountry), bats = as_factor(bats))



pitchers_23_enhanced <- pitchers_23 |>
  left_join(
    mound_22 |>
      rename(PLAYER_ID = pitcher, PENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  left_join(
    mound_21 |>
      rename(PLAYER_ID = pitcher, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  inner_join(
    lahman |>
      select(player_mlb_id, birthYear, birthCountry, weight, height, throws, debut_year),
    by = join_by(PLAYER_ID == player_mlb_id)
  ) |>
  mutate(birthCountry = as_factor(birthCountry), throws = as_factor(throws))
