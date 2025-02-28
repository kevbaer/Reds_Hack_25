library(fastDummies)

Reds_Hitters_FE_24 <- reds_24 |>
  select(
    batter,
    game_pk,
    events,
    hit_distance_sc,
    launch_speed,
    launch_angle,
    estimated_ba_using_speedangle,
    estimated_woba_using_speedangle,
    launch_speed_angle,
    delta_home_win_exp,
    delta_run_exp
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle"),
    remove_selected_columns = TRUE
  ) |>
  group_by(batter) |>
  summarize(
    hit_distance_sc = mean(hit_distance_sc, na.rm = TRUE),
    launch_speed = mean(launch_speed, na.rm = TRUE),
    launch_angle = mean(launch_angle, na.rm = TRUE),
    est_ba = mean(estimated_ba_using_speedangle, na.rm = TRUE),
    est_woba = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    est_wpa = mean(delta_home_win_exp, na.rm = TRUE),
    est_ra = mean(delta_run_exp, na.rm = TRUE),
    double = mean(events_double, na.rm = TRUE),
    hr = mean(events_home_run, na.rm = TRUE),
    single = mean(events_single, na.rm = TRUE),
    K = mean(events_strikeout, na.rm = TRUE),
    triple = mean(events_triple, na.rm = TRUE),
    walk = mean(events_walk, na.rm = TRUE),
    pitches = mean(events_NA, na.rm = TRUE),
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("penult_", .x, recycle0 = TRUE))

Reds_Hitters_FE_23 <- reds_23 |>
  select(
    batter,
    game_pk,
    events,
    hit_distance_sc,
    launch_speed,
    launch_angle,
    estimated_ba_using_speedangle,
    estimated_woba_using_speedangle,
    launch_speed_angle,
    delta_home_win_exp,
    delta_run_exp
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle"),
    remove_selected_columns = TRUE
  ) |>
  group_by(batter) |>
  summarize(
    hit_distance_sc = mean(hit_distance_sc, na.rm = TRUE),
    launch_speed = mean(launch_speed, na.rm = TRUE),
    launch_angle = mean(launch_angle, na.rm = TRUE),
    est_ba = mean(estimated_ba_using_speedangle, na.rm = TRUE),
    est_woba = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    est_wpa = mean(delta_home_win_exp, na.rm = TRUE),
    est_ra = mean(delta_run_exp, na.rm = TRUE),
    double = mean(events_double, na.rm = TRUE),
    hr = mean(events_home_run, na.rm = TRUE),
    single = mean(events_single, na.rm = TRUE),
    K = mean(events_strikeout, na.rm = TRUE),
    triple = mean(events_triple, na.rm = TRUE),
    walk = mean(events_walk, na.rm = TRUE),
    pitches = mean(events_NA, na.rm = TRUE),
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("antepenult_", .x, recycle0 = TRUE))


reds_batter_hyper <- reds_batters_enhanced |>
  left_join(Reds_Hitters_FE_24, by = join_by(MLBID == penult_batter)) |>
  left_join(Reds_Hitters_FE_23, by = join_by(MLBID == antepenult_batter))
