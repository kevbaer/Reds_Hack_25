library(fastDummies)

Pitcher_FE_22 <- savant_22 |>
  select(
    pitcher, game_pk, events, hit_distance_sc, launch_speed, launch_angle,
    estimated_ba_using_speedangle, estimated_woba_using_speedangle,
    launch_speed_angle, delta_home_win_exp, delta_run_exp, pitch_type,
    release_speed, zone, type, pfx_x, pfx_z, release_spin_rate, spin_axis,
    role_key,
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle", "pitch_type", "zone", "type", "role_key"),
    remove_selected_columns = TRUE
  ) |>
  group_by(pitcher) |>
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
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE),
    release_speed = mean(release_speed, na.rm = TRUE),
    pfx_x = mean(pfx_x, na.rm = TRUE),
    pfx_z = mean(pfx_z, na.rm = TRUE),
    release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
    spin_axis = mean(spin_axis, na.rm = TRUE),
    ch_rate = mean(pitch_type_CH, na.rm = TRUE),
    cs_rate = mean(pitch_type_CS, na.rm = TRUE),
    cu_rate = mean(pitch_type_CU, na.rm = TRUE),
    ep_rate = mean(pitch_type_EP, na.rm = TRUE),
    fa_rate = mean(pitch_type_FA, na.rm = TRUE),
    fc_rate = mean(pitch_type_FC, na.rm = TRUE),
    ff_rate = mean(pitch_type_FF, na.rm = TRUE),
    fs_rate = mean(pitch_type_FS, na.rm = TRUE),
    kc_rate = mean(pitch_type_KC, na.rm = TRUE),
    kn_rate = mean(pitch_type_KN, na.rm = TRUE),
    po_rate = mean(pitch_type_PO, na.rm = TRUE),
    si_rate = mean(pitch_type_SI, na.rm = TRUE),
    sl_rate = mean(pitch_type_SL, na.rm = TRUE),
    st_rate = mean(pitch_type_ST, na.rm = TRUE),
    sv_rate = mean(pitch_type_SV, na.rm = TRUE),
    zone_1_rate = mean(zone_1, na.rm = TRUE),
    zone_2_rate = mean(zone_2, na.rm = TRUE),
    zone_3_rate = mean(zone_3, na.rm = TRUE),
    zone_4_rate = mean(zone_4, na.rm = TRUE),
    zone_5_rate = mean(zone_5, na.rm = TRUE),
    zone_6_rate = mean(zone_6, na.rm = TRUE),
    zone_7_rate = mean(zone_7, na.rm = TRUE),
    zone_8_rate = mean(zone_8, na.rm = TRUE),
    zone_9_rate = mean(zone_9, na.rm = TRUE),
    zone_11_rate = mean(zone_11, na.rm = TRUE),
    zone_12_rate = mean(zone_12, na.rm = TRUE),
    zone_13_rate = mean(zone_13, na.rm = TRUE),
    zone_14_rate = mean(zone_14, na.rm = TRUE),
    role_SP = mean(role_key_SP, na.rm = TRUE),
    role_RP = mean(role_key_RP, na.rm = TRUE),
    type_B = mean(type_B, na.rm = TRUE),
    type_S = mean(type_S, na.rm = TRUE),
    type_X = mean(type_X, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("penult_", .x, recycle0 = TRUE))

Pitcher_FE_21 <- savant_21 |>
  select(
    pitcher, game_pk, events, hit_distance_sc, launch_speed, launch_angle,
    estimated_ba_using_speedangle, estimated_woba_using_speedangle,
    launch_speed_angle, delta_home_win_exp, delta_run_exp, pitch_type,
    release_speed, zone, type, pfx_x, pfx_z, release_spin_rate, spin_axis,
    role_key,
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle", "pitch_type", "zone", "type", "role_key"),
    remove_selected_columns = TRUE
  ) |>
  group_by(pitcher) |>
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
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE),
    release_speed = mean(release_speed, na.rm = TRUE),
    pfx_x = mean(pfx_x, na.rm = TRUE),
    pfx_z = mean(pfx_z, na.rm = TRUE),
    release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
    spin_axis = mean(spin_axis, na.rm = TRUE),
    ch_rate = mean(pitch_type_CH, na.rm = TRUE),
    cs_rate = mean(pitch_type_CS, na.rm = TRUE),
    cu_rate = mean(pitch_type_CU, na.rm = TRUE),
    ep_rate = mean(pitch_type_EP, na.rm = TRUE),
    fa_rate = mean(pitch_type_FA, na.rm = TRUE),
    fc_rate = mean(pitch_type_FC, na.rm = TRUE),
    ff_rate = mean(pitch_type_FF, na.rm = TRUE),
    fs_rate = mean(pitch_type_FS, na.rm = TRUE),
    kc_rate = mean(pitch_type_KC, na.rm = TRUE),
    kn_rate = mean(pitch_type_KN, na.rm = TRUE),
    po_rate = mean(pitch_type_PO, na.rm = TRUE),
    si_rate = mean(pitch_type_SI, na.rm = TRUE),
    sl_rate = mean(pitch_type_SL, na.rm = TRUE),
    st_rate = mean(pitch_type_ST, na.rm = TRUE),
    sv_rate = mean(pitch_type_SV, na.rm = TRUE),
    zone_1_rate = mean(zone_1, na.rm = TRUE),
    zone_2_rate = mean(zone_2, na.rm = TRUE),
    zone_3_rate = mean(zone_3, na.rm = TRUE),
    zone_4_rate = mean(zone_4, na.rm = TRUE),
    zone_5_rate = mean(zone_5, na.rm = TRUE),
    zone_6_rate = mean(zone_6, na.rm = TRUE),
    zone_7_rate = mean(zone_7, na.rm = TRUE),
    zone_8_rate = mean(zone_8, na.rm = TRUE),
    zone_9_rate = mean(zone_9, na.rm = TRUE),
    zone_11_rate = mean(zone_11, na.rm = TRUE),
    zone_12_rate = mean(zone_12, na.rm = TRUE),
    zone_13_rate = mean(zone_13, na.rm = TRUE),
    zone_14_rate = mean(zone_14, na.rm = TRUE),
    role_SP = mean(role_key_SP, na.rm = TRUE),
    role_RP = mean(role_key_RP, na.rm = TRUE),
    type_B = mean(type_B, na.rm = TRUE),
    type_S = mean(type_S, na.rm = TRUE),
    type_X = mean(type_X, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("antepenult_", .x, recycle0 = TRUE))



pitchers_23_hyper <- pitchers_23_enhanced |>
  left_join(Pitcher_FE_22, by = join_by(PLAYER_ID == penult_pitcher)) |>
  left_join(Pitcher_FE_21, by = join_by(PLAYER_ID == antepenult_pitcher))


# New ---------------------------------------------------------------------

Ante_Pitcher_FE_22 <- savant_22 |>
  select(
    pitcher, game_pk, events, hit_distance_sc, launch_speed, launch_angle,
    estimated_ba_using_speedangle, estimated_woba_using_speedangle,
    launch_speed_angle, delta_home_win_exp, delta_run_exp, pitch_type,
    release_speed, zone, type, pfx_x, pfx_z, release_spin_rate, spin_axis,
    role_key,
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle", "pitch_type", "zone", "type", "role_key"),
    remove_selected_columns = TRUE
  ) |>
  group_by(pitcher) |>
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
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE),
    release_speed = mean(release_speed, na.rm = TRUE),
    pfx_x = mean(pfx_x, na.rm = TRUE),
    pfx_z = mean(pfx_z, na.rm = TRUE),
    release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
    spin_axis = mean(spin_axis, na.rm = TRUE),
    ch_rate = mean(pitch_type_CH, na.rm = TRUE),
    cs_rate = mean(pitch_type_CS, na.rm = TRUE),
    cu_rate = mean(pitch_type_CU, na.rm = TRUE),
    ep_rate = mean(pitch_type_EP, na.rm = TRUE),
    fa_rate = mean(pitch_type_FA, na.rm = TRUE),
    fc_rate = mean(pitch_type_FC, na.rm = TRUE),
    ff_rate = mean(pitch_type_FF, na.rm = TRUE),
    fs_rate = mean(pitch_type_FS, na.rm = TRUE),
    kc_rate = mean(pitch_type_KC, na.rm = TRUE),
    kn_rate = mean(pitch_type_KN, na.rm = TRUE),
    po_rate = mean(pitch_type_PO, na.rm = TRUE),
    si_rate = mean(pitch_type_SI, na.rm = TRUE),
    sl_rate = mean(pitch_type_SL, na.rm = TRUE),
    st_rate = mean(pitch_type_ST, na.rm = TRUE),
    sv_rate = mean(pitch_type_SV, na.rm = TRUE),
    zone_1_rate = mean(zone_1, na.rm = TRUE),
    zone_2_rate = mean(zone_2, na.rm = TRUE),
    zone_3_rate = mean(zone_3, na.rm = TRUE),
    zone_4_rate = mean(zone_4, na.rm = TRUE),
    zone_5_rate = mean(zone_5, na.rm = TRUE),
    zone_6_rate = mean(zone_6, na.rm = TRUE),
    zone_7_rate = mean(zone_7, na.rm = TRUE),
    zone_8_rate = mean(zone_8, na.rm = TRUE),
    zone_9_rate = mean(zone_9, na.rm = TRUE),
    zone_11_rate = mean(zone_11, na.rm = TRUE),
    zone_12_rate = mean(zone_12, na.rm = TRUE),
    zone_13_rate = mean(zone_13, na.rm = TRUE),
    zone_14_rate = mean(zone_14, na.rm = TRUE),
    role_SP = mean(role_key_SP, na.rm = TRUE),
    role_RP = mean(role_key_RP, na.rm = TRUE),
    type_B = mean(type_B, na.rm = TRUE),
    type_S = mean(type_S, na.rm = TRUE),
    type_X = mean(type_X, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("antepenult_", .x, recycle0 = TRUE))

Penult_Pitcher_FE_23 <- savant_23 |>
  select(
    pitcher, game_pk, events, hit_distance_sc, launch_speed, launch_angle,
    estimated_ba_using_speedangle, estimated_woba_using_speedangle,
    launch_speed_angle, delta_home_win_exp, delta_run_exp, pitch_type,
    release_speed, zone, type, pfx_x, pfx_z, release_spin_rate, spin_axis,
    role_key,
  ) |>
  mutate(launch_speed_angle = as_factor(launch_speed_angle)) |>
  dummy_cols(
    select_columns = c("events", "launch_speed_angle", "pitch_type", "zone", "type", "role_key"),
    remove_selected_columns = TRUE
  ) |>
  group_by(pitcher) |>
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
    launch_1 = mean(launch_speed_angle_1, na.rm = TRUE),
    launch_2 = mean(launch_speed_angle_2, na.rm = TRUE),
    launch_3 = mean(launch_speed_angle_3, na.rm = TRUE),
    launch_4 = mean(launch_speed_angle_4, na.rm = TRUE),
    launch_5 = mean(launch_speed_angle_5, na.rm = TRUE),
    launch_6 = mean(launch_speed_angle_6, na.rm = TRUE),
    release_speed = mean(release_speed, na.rm = TRUE),
    pfx_x = mean(pfx_x, na.rm = TRUE),
    pfx_z = mean(pfx_z, na.rm = TRUE),
    release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
    spin_axis = mean(spin_axis, na.rm = TRUE),
    ch_rate = mean(pitch_type_CH, na.rm = TRUE),
    cs_rate = mean(pitch_type_CS, na.rm = TRUE),
    cu_rate = mean(pitch_type_CU, na.rm = TRUE),
    ep_rate = mean(pitch_type_EP, na.rm = TRUE),
    fa_rate = mean(pitch_type_FA, na.rm = TRUE),
    fc_rate = mean(pitch_type_FC, na.rm = TRUE),
    ff_rate = mean(pitch_type_FF, na.rm = TRUE),
    fs_rate = mean(pitch_type_FS, na.rm = TRUE),
    kc_rate = mean(pitch_type_KC, na.rm = TRUE),
    kn_rate = mean(pitch_type_KN, na.rm = TRUE),
    po_rate = mean(pitch_type_PO, na.rm = TRUE),
    si_rate = mean(pitch_type_SI, na.rm = TRUE),
    sl_rate = mean(pitch_type_SL, na.rm = TRUE),
    st_rate = mean(pitch_type_ST, na.rm = TRUE),
    sv_rate = mean(pitch_type_SV, na.rm = TRUE),
    zone_1_rate = mean(zone_1, na.rm = TRUE),
    zone_2_rate = mean(zone_2, na.rm = TRUE),
    zone_3_rate = mean(zone_3, na.rm = TRUE),
    zone_4_rate = mean(zone_4, na.rm = TRUE),
    zone_5_rate = mean(zone_5, na.rm = TRUE),
    zone_6_rate = mean(zone_6, na.rm = TRUE),
    zone_7_rate = mean(zone_7, na.rm = TRUE),
    zone_8_rate = mean(zone_8, na.rm = TRUE),
    zone_9_rate = mean(zone_9, na.rm = TRUE),
    zone_11_rate = mean(zone_11, na.rm = TRUE),
    zone_12_rate = mean(zone_12, na.rm = TRUE),
    zone_13_rate = mean(zone_13, na.rm = TRUE),
    zone_14_rate = mean(zone_14, na.rm = TRUE),
    role_SP = mean(role_key_SP, na.rm = TRUE),
    role_RP = mean(role_key_RP, na.rm = TRUE),
    type_B = mean(type_B, na.rm = TRUE),
    type_S = mean(type_S, na.rm = TRUE),
    type_X = mean(type_X, na.rm = TRUE)
  ) |>
  rename_with(~ paste0("penult_", .x, recycle0 = TRUE))
