# helper function to cleanup brfss data
cleanup_brfss <- function(data){

  # define columns containing numeric data
  numeric_vars <- c("reported_weight_in_kilograms",
                    "reported_height_in_meters",
                    "how_many_times_did_you_eat_fruit_per_month",
                    "how_many_times_per_month_walking_running_etc",
                    "hours_walking_running_jogging_or_swimming",
                    "how_many_times_did_you_eat_dark_green_vegetables_per_week",
                    "days_in_past_30_had_alcoholic_beverage")

  data |>
    clean_names() |>
    # set unknown/missing values to NA
    mutate(across(where(is.character), na_if, "Refused"),
           across(where(is.character), na_if, "Don’t know/Not sure"),
           across(where(is.character), na_if, "Don’t know/Not Sure"),
           across(where(is.character), na_if, "Don´t know/Not Sure"),
           across(where(is.character), na_if, "Not asked or missing"),
           across(where(is.character), na_if, "Not asked or Missing"),
    ) |>
    mutate(
      across(any_of(c("age_when_told_had_diabetes")), str_remove, " years" )
    ) |>
    # convert columns to numeric
    mutate(across(any_of(numeric_vars), as.numeric)) |>
    mutate(across(where(is.character), ~as_factor(.)))
}

