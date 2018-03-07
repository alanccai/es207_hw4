annual_daily_mean <- function(df) {
  annual <- df %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%                            # groups by year
    summarize(Mean_o3 = mean(o3, na.rm = TRUE))   # daily mean by year
  an_mean <- mean(annual$Mean_o3)                 # mean of daily mean by year
  return(an_mean)
}