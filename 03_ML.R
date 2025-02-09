source("02_prep.R")

library(tidymodels)
library(baguette)
library(rules)
library(stacks)


set.seed(11042004)
hitter_split <- initial_split(batters_23_enhanced, strata = ACTUAL_TIME)
hitter_train <- training(hitter_split)
hitter_test <- testing(hitter_split)

hitter_folds <- vfold_cv(hitter_train, strata = ACTUAL_TIME, repeats = 5)

rec_1 <-
  recipe(ACTUAL_TIME ~ ., data = hitter_train) |>
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
    grid = 25,
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
  extract_workflow_set_result("rec_RF") %>%
  select_best(metric = "rmse")

boosting_test_results <-
  grid_results %>%
  extract_workflow("rec_RF") %>%
  finalize_workflow(best) %>%
  last_fit(split = hitter_split)
collect_metrics(boosting_test_results)

rf_wf <-
  workflow() |>
  add_recipe(rec_1) |>
  add_model(rf_spec)

hitter_mod <- finalize_workflow(
  rf_wf,
  best
) |>
  fit(data = hitter_train)
