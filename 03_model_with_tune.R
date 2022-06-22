source("00_packages.R")


# qs-files zijn al gecleaned bij de explore
data <- qread("data/brfss/brfss_level_1.qs") |> filter(!is.na(general_health))
#data <- qread("data/brfss/brfss_level_2.qs") |> filter(!is.na(general_health))

# maak 2 levels van general_heath
data <- data |> mutate(
  general_health = case_when(
    general_health %in% c("Poor", "Fair") ~ "slecht",
    TRUE ~ "goed"
  ))

set.seed(123)
data_split <- initial_split(data, strata = general_health)
train_data <- training(data_split)
test_data <- testing(data_split)
folds <- vfold_cv(train_data, v = 10, repeats = 1, strata = general_health)


# define recipe for classification of general health ----------------------

gh_rec <- recipe(general_health ~ . , data = train_data) |>
  update_role(id , new_role = "id") |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_unknown((all_nominal_predictors())) |>
  step_other(all_nominal_predictors()) |>
  step_relevel(have_any_health_care_coverage, ref_level = "Yes") |>
  step_relevel(could_not_see_doctor_because_of_cost, ref_level = "No") |>
  step_relevel(smoked_at_least_100_cigarettes, ref_level = "No") |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors()) |>
  step_normalize() |>
  # make dataset balanced
  #step_smote(general_health) |>
  prep()


# define
cart_spec <- decision_tree(
  cost_complexity = tune(),
  min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


rf_wf <- workflow() %>%
  add_recipe(gh_rec) %>%
  add_model(rf_spec)


doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = 10
)

tune_res


tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry, trees) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

best_auc <- select_best(tune_res, "roc_auc")

# maak een final rf_model met de beste tuning parameters
final_rf_model <- finalize_model(
  rf_spec,
  best_auc
)

final_rf_model

# Now make a final workflow
final_rf_wf <- workflow() |>
  add_recipe(gh_rec) |>
  add_model(final_rf_model)

# last_fit traint een model op alle trainingsdata en evalueert op de testdata
final_res <- final_rf_wf %>%
  last_fit(data_split)


final_res %>%
  collect_metrics()

# confusion matrix
final_res %>%
  collect_predictions() %>%
  conf_mat(truth = general_health,
           estimate = .pred_class)

# print de ROC curve
final_res |>
  collect_predictions() |>
  roc_curve(truth = general_health,
            estimate = .pred_goed) |>
  autoplot()



# extract the workflow of final model
gh_wf_model <- final_res$.workflow[[1]]

tmp <- augment(gh_wf_model, test_data)
count(tmp, .pred_class)
