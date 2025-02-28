library(googlesheets4)

url_start <- "https://docs.google.com/spreadsheets/d/"
url_end <- "10qPhj3-V1MJR4mwQmMx9ZaLNrQsi5uedXDyl5eNGvn0/edit?gid=0#gid=0"

gs4_auth(email = "kmb114410@gmail.com")
drive_file <-
  as_sheets_id(paste0(url_start, url_end))

reds_batter_hyper <- reds_batter_hyper |>
  mutate(PLAYER_ID = as.character(MLBID)) |>
  select(-MLBID)

reds_batter_hyper$prediction <- predict(hitter_mod, reds_batter_hyper)$.pred

reds_batter_final <- reds_batter_hyper |>
  mutate(prediction = round(prediction)) |>
  select(names, prediction)


reds_pitcher_hyper_z <- reds_pitchers_hyper |>
  mutate(PLAYER_ID = as.character(MLBID)) |>
  select(-MLBID)


reds_pitcher_hyper_z$prediction <-
  predict(final_mod, reds_pitcher_hyper_z)$.pred

reds_pitcher_final <- reds_pitcher_hyper_z |>
  mutate(prediction = round(prediction)) |>
  select(names, prediction)


reds_final_preds <- reds_batter_final |>
  bind_rows(reds_pitcher_final)

# sheet_write(reds_final_preds, ss = drive_file, sheet = "final_preds")
