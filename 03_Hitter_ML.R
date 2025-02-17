source("02_prep.R")
source("06_Batter_FE.R")

library(tidymodels)



set.seed(11042004)
hitter_split <- initial_split(batters_23_hyper, strata = ACTUAL_TIME)
hitter_train <- training(hitter_split)
hitter_test <- testing(hitter_split)

hitter_folds <- vfold_cv(hitter_train, strata = ACTUAL_TIME, repeats = 5)

rec_1 <-
  recipe(ACTUAL_TIME ~ ., data = hitter_train) |>
  update_role(PLAYER_ID, new_role = "ID") |>
  step_dummy(all_nominal_predictors())

xgb_spec <-
  boost_tree(
    tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
    min_n = tune(), sample_size = tune(), trees = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")


set <-
  workflow_set(
    preproc = list(rec = rec_1),
    models = list(
      boosting = xgb_spec
    )
  )
doParallel::registerDoParallel(cores = 5)

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )
grid_results <-
  set |>
  workflow_map(
    seed = 11042004,
    resamples = hitter_folds,
    grid = 50,
    control = grid_ctrl
  )
grid_results

grid_results |>
  rank_results() |>
  filter(.metric == "rmse") |>
  select(model, .config, rmse = mean, rank)
beepr::beep("fanfare")

autoplot(
  grid_results,
  rank_metric = "rmse", # <- how to order models
  metric = "rmse", # <- which metric to visualize
  select_best = TRUE # <- one point per workflow
) +
  geom_text(aes(y = mean - 1 / 2, label = wflow_id), angle = 90, hjust = 1) +
  theme(legend.position = "none")


best <-
  grid_results %>%
  extract_workflow_set_result("rec_boosting") %>%
  select_best(metric = "rmse")

boosting_test_results <-
  grid_results %>%
  extract_workflow("rec_boosting") %>%
  finalize_workflow(best) %>%
  last_fit(split = hitter_split)
collect_metrics(boosting_test_results)

xgb_hitter_wf <-
  workflow() |>
  add_recipe(rec_1) |>
  add_model(xgb_spec)

hitter_mod <- finalize_workflow(
  xgb_hitter_wf,
  best
) |>
  fit(data = batters_23_hyper)
