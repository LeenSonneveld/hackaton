source("00_packages.R")

doParallel::registerDoParallel()

# qs-files zijn al gecleaned bij de explore
data <- qread("data/brfss/brfss_level_1.qs")
data <- qread("data/brfss/brfss_level_2.qs")

data <- data |>
  filter(!is.na(general_health), general_health != "Don’t know/Not Sure")

# maak 2 levels van general_heath
data <- data |> mutate(
  general_health = case_when(
    general_health %in% c("Poor", "Fair") ~ "ongezond",
    TRUE ~ "gezond"
  ))


count(data, general_health, sex_of_respondent)

data$new = data$ko * 2
data <- data |>
  mutate(
    new = k *2 + 1)

set.seed(123)
data_split <- initial_split(data, strata = general_health)
train_data <- training(data_split)
test_data <- testing(data_split)
folds <- vfold_cv(train_data, v = 3, repeats = 1, strata = general_health)


# define recipe for classification of general health ----------------------

edu_levels <- c("1 No education", "2 Elementary","3 Some high school",
                "4 High school graduate", "5 Some college or technical school" )

gh_rec <- recipe(general_health ~ . , data = train_data) |>
  update_role(id , new_role = "id") |>
  step_mutate(education_level = as.numeric(substr(education_level, 1, 1))) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_mutate(bmi = reported_weight_in_kilograms / reported_height_in_meters ^2) |>
  step_ordinalscore() |>
  step_unknown((all_nominal_predictors())) |>
  step_other(all_nominal_predictors()) |>
  step_relevel(have_any_health_care_coverage, ref_level = "Yes") |>
  step_relevel(could_not_see_doctor_because_of_cost, ref_level = "No") |>
  step_relevel(smoked_at_least_100_cigarettes, ref_level = "No") |>
  step_dummy(all_nominal_predictors()) |>
  step_nzv(all_predictors()) |>
  step_normalize() |>
  # make dataset balanced
  step_upsample(general_health) |>
  prep()


# hoe ziet de data voor het model eruit? nog NA?
baked_data <- bake(gh_rec, new_data = train_data)

# define model specification
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |>
  set_mode("classification") |>
  set_engine("ranger")


rf_wf <- workflow() |>
  add_recipe(gh_rec) |>
  add_model(rf_spec)

# decision tree
dt_spec <- decision_tree() |>
  set_mode("classification") |>
  set_engine("rpart")




dt_wf <- workflow() |>
  add_recipe(gh_recipe) |>
  add_model(dt_spec)

set.seed(345)
tune_res <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = 10
)

tune_res


tune_res |>
  collect_metrics()  |>
  filter(.metric == "roc_auc")  |>
  select(mean, min_n, mtry, trees)  |>
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter"
  ) |>
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
final_res <- final_rf_wf  |>
  last_fit(data_split)


final_res |>
  collect_metrics()

# confusion matrix
final_res |>
  collect_predictions()  |>
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



# Workflow set ----

# start parallel
folds <- vfold_cv(train_data, v = 3, repeats = 1, strata = general_health)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) |>
  set_mode("classification")  |>
  set_engine("ranger")


dt_spec <- decision_tree(
  cost_complexity = tune(),
  min_n = tune()
) |>
  set_mode("classification")  |>
  set_engine("rpart")


glm_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_mode("classification") |>
  set_engine("glmnet")


# define worflows
all_workflows <- workflow_set(
  preproc =  list(
    "gh_rec" = gh_rec),
  models = list(
    "random_forest" = rf_spec,
    "decision_tree" = dt_spec,
    "glmnet" = glm_spec
  )
)


all_workflows

all_workflows_tuned <- all_workflows |>
  workflow_map(resamples = folds, grid = 5, verbose = TRUE)

all_workflows_tuned


rank_results(all_workflows_tuned, rank_metric = "roc_auc")

autoplot(all_workflows_tuned, metric = "roc_auc")

autoplot(all_workflows_tuned, metric = "roc_auc", id = "gh_rec_glmnet")
autoplot(all_workflows_tuned, metric = "roc_auc", id = "gh_rec_random_forest")
