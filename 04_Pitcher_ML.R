source("02_prep.R")

library(tidymodels)
library(baguette)
library(rules)
library(stacks)


set.seed(11042004)
pitcher_split <- initial_split(pitchers_23_enhanced, strata = ACTUAL_TIME)
pitcher_train <- training(pitcher_split)
pitcher_test <- testing(pitcher_split)

pitcher_folds <- vfold_cv(pitcher_train, strata = ACTUAL_TIME, repeats = 5)

rec_2 <-
  recipe(ACTUAL_TIME ~ ., data = pitcher_train) |>
  update_role(PLAYER_ID, new_role = "ID") |>
  step_dummy(all_nominal_predictors())


nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
  set_engine("nnet", MaxNWts = 2600) |>
  set_mode("regression")

nnet_param <-
  nnet_spec %>%
  extract_parameter_set_dials() %>%
  update(hidden_units = hidden_units(c(1, 27)))

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

bag_cart_spec <-
  bag_tree() |>
  set_engine("rpart", times = 50L) |>
  set_mode("regression")

rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_engine("ranger") |>
  set_mode("regression")

xgb_spec <-
  boost_tree(
    tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
    min_n = tune(), sample_size = tune(), trees = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")

cubist_spec <-
  cubist_rules(committees = tune(), neighbors = tune()) |>
  set_engine("Cubist")

set <-
  workflow_set(
    preproc = list(rec = rec),
    models = list(
      neural_network = nnet_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
      RF = rf_spec, boosting = xgb_spec, Cubist = cubist_spec
    )
  )
doParallel::registerDoParallel(cores = 5)

grid_ctrl_2 <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )
grid_results_2 <-
  set |>
  workflow_map(
    seed = 11042004,
    resamples = pitcher_folds,
    grid = 25,
    control = grid_ctrl_2
  )
grid_results_2

grid_results_2 |>
  rank_results() |>
  filter(.metric == "rmse") |>
  select(model, .config, rmse = mean, rank)
beepr::beep("fanfare")

autoplot(
  grid_results_2,
  rank_metric = "rmse", # <- how to order models
  metric = "rmse", # <- which metric to visualize
  select_best = TRUE # <- one point per workflow
) +
  geom_text(aes(y = mean - 1 / 2, label = wflow_id), angle = 90, hjust = 1) +
  theme(legend.position = "none")


best_2 <-
  grid_results_2 %>%
  extract_workflow_set_result("rec_boosting") %>%
  select_best(metric = "rmse")

boosting_test_results_2 <-
  grid_results_2 %>%
  extract_workflow("rec_boosting") %>%
  finalize_workflow(best_2) %>%
  last_fit(split = pitcher_split)
collect_metrics(boosting_test_results_2)

xgb_wf <-
  workflow() |>
  add_recipe(rec_2) |>
  add_model(xgb_spec)

final_mod <- finalize_workflow(
  xgb_wf,
  best_2
) |>
  fit(data = pitcher_train)
