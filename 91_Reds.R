library(googlesheets4)
library(Lahman)

idmap <- read_csv("data/playeridmap.csv") |>
  select(LASTCOMMAFIRST, MLBID, IDPLAYER)

reds_Lahman <- as_tibble(Lahman::People) |>
  mutate(debut_year = year(debut)) |>
  select(
    playerID,
    birthYear,
    birthCountry,
    weight,
    height,
    bats,
    throws,
    debut_year
  )

reds_data <- read_csv("data/reds_data.csv") |>
  mutate(
    NAME_LAST_FIRST = stringi::stri_trans_general(
      NAME_LAST_FIRST,
      "Latin-ASCII"
    )
  )


url_start <- "https://docs.google.com/spreadsheets/d/"
url_end <- "10qPhj3-V1MJR4mwQmMx9ZaLNrQsi5uedXDyl5eNGvn0/edit?gid=0#gid=0"

gs4_auth(email = "kmb114410@gmail.com")
drive_file <-
  as_sheets_id(paste0(url_start, url_end))


reds_players <- tibble(names = unique(reds_data$NAME_LAST_FIRST))

# sheet_write(reds_players, ss = drive_file, sheet = "reds_players")

reds_players <- read_sheet(ss = drive_file, sheet = "reds_players") |>
  mutate(names = stringi::stri_trans_general(names, "Latin-ASCII"))

reds_pitchers <- reds_players |>
  filter(position == "Pitcher") |>
  left_join(idmap, by = join_by("names" == "LASTCOMMAFIRST")) |>
  left_join(reds_Lahman, by = join_by("IDPLAYER" == "playerID")) |>
  filter(!is.na(throws)) |>
  filter(names != "Phillips, Connor") |>
  select(-bats)

reds_batters <- reds_players |>
  filter(position == "Batter") |>
  left_join(idmap, by = join_by("names" == "LASTCOMMAFIRST")) |>
  left_join(reds_Lahman, by = join_by("IDPLAYER" == "playerID")) |>
  filter(!is.na(bats)) |>
  filter(names != "McLain, Matt") |>
  select(-throws)


reds_names <- c(reds_batters$names, reds_pitchers$names)


reds_pitch_22 <- reds_data |>
  filter(game_year == 2022) |>
  filter(NAME_LAST_FIRST %in% reds_pitchers$names) |>
  filter(game_year == 2024) |>
  filter(NAME_LAST_FIRST %in% reds_pitchers$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST,
  ) |>
  summarize(playing_time = n()) |>
  ungroup()

reds_hit_22 <- reds_data |>
  filter(game_year == 2022) |>
  filter(NAME_LAST_FIRST %in% reds_batters$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST
  ) |>
  summarize(
    playing_time = n()
  ) |>
  ungroup()


reds_pitch_23 <- reds_data |>
  filter(game_year == 2023) |>
  filter(NAME_LAST_FIRST %in% reds_pitchers$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST,
  ) |>
  summarize(playing_time = n()) |>
  ungroup()

reds_hit_23 <- reds_data |>
  filter(game_year == 2023) |>
  filter(NAME_LAST_FIRST %in% reds_batters$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST
  ) |>
  summarize(
    playing_time = n()
  ) |>
  ungroup()


reds_pitch_24 <- reds_data |>
  filter(game_year == 2024) |>
  filter(NAME_LAST_FIRST %in% reds_pitchers$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST,
  ) |>
  summarize(playing_time = n()) |>
  ungroup()

reds_hit_24 <- reds_data |>
  filter(game_year == 2024) |>
  filter(NAME_LAST_FIRST %in% reds_batters$names) |>
  group_by(
    NAME_LAST_FIRST,
    game_pk,
    at_bat_number
  ) |>
  summarize() |>
  ungroup() |>
  group_by(
    NAME_LAST_FIRST
  ) |>
  summarize(
    playing_time = n()
  ) |>
  ungroup()


reds_batters_enhanced <- reds_batters |>
  left_join(
    reds_hit_24 |>
      rename(PENULT = playing_time),
    by = join_by("names" == "NAME_LAST_FIRST")
  ) |>
  left_join(
    reds_hit_23 |>
      rename(ANTEPENULT = playing_time),
    by = join_by("names" == "NAME_LAST_FIRST")
  ) |>
  mutate(birthCountry = as_factor(birthCountry), bats = as_factor(bats))


reds_pitchers_enhanced <- reds_pitchers |>
  left_join(
    reds_pitch_24 |>
      rename(PENULT = playing_time),
    by = join_by("names" == "NAME_LAST_FIRST")
  ) |>
  left_join(
    reds_pitch_23 |>
      rename(ANTEPENULT = playing_time),
    by = join_by("names" == "NAME_LAST_FIRST")
  ) |>
  mutate(birthCountry = as_factor(birthCountry), throws = as_factor(throws))
