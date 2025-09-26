si <- read_delim("H:/Import/9565_20240919_Stringency index Netherlands.csv", delim = ";")

si <- si %>%
  mutate(start_date = as.Date(Day, format = "%d/%m/%Y"))


si_fun <- function(df, column_name) {
  
  # Create empty data frame to store the selected rows
  df$end_date <- NA
  selected_rows <- data.frame()
  selected_rows <- rbind(df[1,])
  
  # Loop through the specified column holding the stringency value starting from the second row
  for (i in 2:nrow(df)) {
    if (df[i, column_name] != df[i-1, column_name]) {
      selected_rows <- rbind(selected_rows, df[i, ])
    }
  }
  
  for (i in 1:nrow(selected_rows)) {
    
    selected_rows$end_date[i] <- selected_rows$start_date[i + 1] 
  
  }
  
  
  return(selected_rows)
}

si_df <- si_fun(si, "Stringency index (weighted average)")



si_df <- si_df %>%
  mutate(end_date = ifelse(is.na(end_date), as.Date("2022-01-01"), end_date),
         end_date = as.Date(end_date),
         si = `Stringency index (weighted average)`) %>%
  select(start_date, end_date, si)

si_col_custom <- c("#e54339", "#F28220", "#FFC107","#8BC061", "#17bebb") #








