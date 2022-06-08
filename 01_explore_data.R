#' doet een exploratie van de data en schrijf resultaten weg in
#' de reports directory

source("00_packages.R")



# brfss level1 ------------------------------------------------------------


brfss_level_1 <- read_delim("data/brfss/brfss_level_1.csv",
                            delim = "|",
                            escape_double = FALSE,
                            trim_ws = TRUE) |>
  clean_names()

brfss_level_1 %>%
  #slice_sample(n = 100) |>
  create_report(
    output_file = paste0("explore_brfss_level_1", format(Sys.time(), "%Y%m%d")),
    output_dir = "reports",
    report_title = "EDA Report - brffs_level_1",
    y = "general_health",
    config = configure_report(
      add_plot_prcomp = FALSE,
      add_plot_boxplot = FALSE,
      add_plot_qq = FALSE,
      add_plot_scatterplot = FALSE
    )
  )




# brfss level2 ------------------------------------------------------------


brfss_level_2 <- read_delim("data/brfss/brfss_level_2.csv",
                            delim = "|",
                            escape_double = FALSE,
                            trim_ws = TRUE) |>
  clean_names()

brfss_level_2 |>
  #slice_sample(n = 100) |>
  create_report(
    output_file = paste0("explore_brfss_level_2", format(Sys.time(), "%Y%m%d")),
    output_dir = "reports",
    report_title = "EDA Report - brffs_level_2",
    y = "general_health",
    config = configure_report(
      add_plot_prcomp = FALSE,
      add_plot_boxplot = FALSE,
      add_plot_qq = FALSE,
      add_plot_scatterplot = FALSE
    )
  )

