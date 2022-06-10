# helper function to cleanup brfss data
cleanup_brfss <- function(data){

  # define columns containing numeric data
  numeric_vars <- c("reported_weight_in_kilograms",
                    "reported_height_in_meters",
                    "how_many_times_did_you_eat_fruit_per_month",
                    "how_many_times_per_month_walking_running_etc",
                    "hours_walking_running_jogging_or_swimming",
                    "how_many_times_did_you_eat_dark_green_vegetables_per_week",
                    "days_in_past_30_had_alcoholic_beverage",
                    "age_when_told_had_diabetes",
                    "number_of_children_in_household")


  # deze waarde worden vervangen door NA
  na_values <- c("Refused",
                 "Don’t know/Not sure",
                 "Don’t know/Not Sure",
                 "Don´t Know/Not Sure",
                 "Don´t know/Not Sure",
                 "Not asked or missing",
                 "Not asked or Missing",
                 "Don’t know/Refused/Missing",
                 "Don’t know/Not Sure/Refused/Missing",
                 "Don’t know/Not sure/Refused")

  data |>
    clean_names() |>
    # set unknown/missing values to NA
    mutate(across(where(is.character), ~if_else(. %in% na_values, NA_character_, .))
    ) |>
    # in kolomnaam staan bijv.  y year of x childeren, hiervan alleen het getal
    mutate(
      across(any_of(c("age_when_told_had_diabetes")), str_extract, "[:digit:]" ),
      across(any_of(c("number_of_children_in_household")), str_extract, "[:digit:]" )
    ) |>
    # convert columns to numeric
    mutate(across(any_of(numeric_vars), as.numeric)) |>
    mutate(across(where(is.character), ~as_factor(.)))
}

