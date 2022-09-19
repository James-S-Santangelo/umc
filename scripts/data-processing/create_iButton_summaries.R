library(tidyverse)

# Get all csv files in inpath
inpath <- "data-clean/iButton_csvs/"
files <- dir(inpath, pattern = "*.csv", recursive = TRUE)

# read in all the files, appending the path before the filename
df_list <- files %>%
  map(~ read_csv(file.path(inpath, .), show_col_types = FALSE))

merged_iButton_summaries <- purrr::map_dfr(df_list, summarise_iButtons) %>%

  # Remove duplicated observations
  # Only for Erindale since First and Third observation rounds correspond to the same data
  group_by(Button) %>%
  distinct(week_date, .keep_all = TRUE)

write_csv(merged_iButton_summaries, "data-clean/iButton_summaries.csv")
