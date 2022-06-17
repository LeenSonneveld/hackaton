source("00_packages.R")


# qs-files zijn al gecleaned bij de explore
data <- qread("data/brfss/brfss_level_1.qs") |> filter(!is.na(general_health))
#data <- qread("data/brfss/brfss_level_2.qs") |> filter(!is.na(general_health))

set.seed(123)
data_split <- initial_split(data, strata = general_health)
train_data <- training(data_split)
test_data <- testing(data_split)
folds <- vfold_cv(train_data, v = 10, repeats = 1, strata = general_health)

count(train_data, general_health)


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


summary(gh_rec)

# hoe ziet de data voor het model eruit? nog NA?
baked_data <- bake(gh_rec, new_data = train_data)
map_dbl(baked_data, ~sum(is.na(.)))


# random forest classification
rf_class <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# decision tree classification
dt_class <- decision_tree(
  mode ="classification",
  engine = "rpart")

# make workflow for general_health, using random forest
base_wf <- workflow() |>
  add_recipe(gh_rec)

# fit models ----------

# fit randomforest workflow on train_data
fit_rf_gh <- base_wf |>
  add_model(rf_class) |>
  fit(train_data)


# fit decision tree on train_data
fit_dt_gh <- base_wf |>
  add_model(dt_class) |>
  fit(train_data)

# Evaluate performance on train data ------

# voeg de voorspellingen toe aan de trainingdata
train_data <- train_data |>
  mutate(.pred_rf = predict(fit_rf_gh, train_data)[[".pred_class"]],
         .pred_dt = predict(fit_dt_gh, train_data)[[".pred_class"]]
         )

train_data |>
  select(general_health, starts_with(".pred")) |>
  View()

accuracy(train_data, general_health, .pred_rf)
accuracy(train_data, general_health, .pred_dt)


# voeg de RF voorspelling toe aan de testdata
test_data_rf <- augment(fit_rf_gh, test_data)
test_data_rf |> select(general_health,  starts_with(".pred"))

# voeg de Decision Tree voorspelling toe aan de testdata
test_data_dt <- augment(fit_dt_gh, test_data)
test_data_dt |> select(general_health,  starts_with(".pred"))

# hoe goed is de accuracy op de testdata
accuracy(test_data_rf, truth = general_health, estimate = .pred_class)
accuracy(test_data_dt, truth = general_health, estimate = .pred_class)


precision(train_data, truth = general_health, estimate = .pred_rf)
recall(train_data, truth = general_health, estimate = .pred_rf)




rf_class %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(
    general_health ~ .,
    data = bake(gh_rec, new_data = train_data)
  ) %>%
  vip(geom = "point")


tree <- rpart(general_health ~ ., data = baked_data)
(vi_tree <- tree$variable.importance)
barplot(vi_tree, horiz = TRUE, las = 1)

