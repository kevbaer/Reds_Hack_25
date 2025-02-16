hitters_24 <- sample_sub |>
  select(-PLAYING_TIME) |>
  left_join(
    plate_appearances |>
      rename(PLAYER_ID = batter, PENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  left_join(
    plate_22 |>
      rename(PLAYER_ID = batter, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  filter(!is.na(PENULT) | !is.na(ANTEPENULT)) |>
  left_join(
    lahman |>
      select(player_mlb_id, birthYear, birthCountry, weight, height, bats, debut_year),
    by = join_by(PLAYER_ID == player_mlb_id)
  ) |>
  mutate(birthCountry = as_factor(birthCountry), bats = as_factor(bats)) |>
  left_join(Penult_Hitter_FE_23, by = join_by(PLAYER_ID == penult_batter)) |>
  left_join(Ante_Hitter_FE_22, by = join_by(PLAYER_ID == antepenult_batter))



hitters_24$PLAYING_TIME <- predict(hitter_mod, hitters_24)$.pred



pitchers_24 <- sample_sub |>
  select(-PLAYING_TIME) |>
  left_join(
    batters_face_average |>
      rename(PLAYER_ID = pitcher, PENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  left_join(
    mound_22 |>
      rename(PLAYER_ID = pitcher, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  filter(!is.na(PENULT) | !is.na(ANTEPENULT)) |>
  left_join(
    lahman |>
      select(player_mlb_id, birthYear, birthCountry, weight, height, throws, debut_year),
    by = join_by(PLAYER_ID == player_mlb_id)
  ) |>
  mutate(birthCountry = as_factor(birthCountry), throws = as_factor(throws)) |>
  left_join(Penult_Pitcher_FE_23, by = join_by(PLAYER_ID == penult_pitcher)) |>
  left_join(Ante_Pitcher_FE_22, by = join_by(PLAYER_ID == antepenult_pitcher))


pitchers_24$PLAYING_TIME <- predict(final_mod, pitchers_24)$.pred




pitchers_NAd <- zz |>
  filter(!y23 & !y22) |>
  select(-c(PLAYING_TIME, y23, y22, y21, n, bats)) |>
  left_join(
    mound_21 |>
      rename(PLAYER_ID = pitcher, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  mutate(PENULT = NA) |>
  filter(!is.na(ANTEPENULT)) |>
  mutate(birthCountry = as_factor(birthCountry), throws = as_factor(throws)) |>
  left_join(Pitcher_FE_21, by = join_by(PLAYER_ID == antepenult_pitcher)) |>
  left_join(Penult_Pitcher_FE_23, by = join_by(PLAYER_ID == penult_pitcher))

pitchers_NAd$PLAYING_TIME <- (predict(final_mod, pitchers_NAd)$.pred) * 2 / 3

hitters_NAd <- zz |>
  filter(!y23 & !y22) |>
  select(-c(PLAYING_TIME, y23, y22, y21, n, throws)) |>
  left_join(
    plate_21 |>
      rename(PLAYER_ID = batter, ANTEPENULT = playing_time),
    by = "PLAYER_ID"
  ) |>
  mutate(PENULT = NA) |>
  filter(!is.na(ANTEPENULT)) |>
  mutate(birthCountry = as_factor(birthCountry), bats = as_factor(bats)) |>
  left_join(Hitter_FE_21, by = join_by(PLAYER_ID == antepenult_batter)) |>
  left_join(Penult_Hitter_FE_23, by = join_by(PLAYER_ID == penult_batter))

hitters_NAd$PLAYING_TIME <- (predict(hitter_mod, hitters_NAd)$.pred) * 2 / 3




PREDS <- hitters_24 |>
  select(PLAYER_ID, PLAYING_TIME) |>
  bind_rows(pitchers_24 |>
    select(PLAYER_ID, PLAYING_TIME)) |>
  bind_rows(pitchers_NAd |>
    select(PLAYER_ID, PLAYING_TIME)) |>
  bind_rows(hitters_NAd |>
    select(PLAYER_ID, PLAYING_TIME)) |>
  group_by(PLAYER_ID) |>
  summarize(PLAYING_TIME = sum(PLAYING_TIME)) |>
  ungroup()

write_csv(PREDS, file = "submissions/submission_2-15-25_FINAL.csv")
