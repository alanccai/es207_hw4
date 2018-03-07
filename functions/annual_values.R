annual_values <- function(df) {
  annual <- df %>%
    filter(str_detect(`Site Name`, "^(San|Santa)") == TRUE) %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year, `Site Name`) %>%
    summarize(Mean = mean(o3, na.rm = TRUE),
              Median = median(o3, na.rm = TRUE),
              Max = max(o3, na.rm = TRUE),
              Min = min(o3, na.rm = TRUE))
  return(annual)
}
