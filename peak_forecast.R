library(tidyverse)

make_peak_forecasts <- function(
  case_counts, date_information
) {
  
  round_id <- first(date_information$round_id)
  
  # Filter for just this season
  case_counts_yearly <- case_counts %>% 
    filter(pathogen != "SARSCOV2")%>%
    mutate(year = floor_date(notification_date, "year")) %>%
    filter(notification_date < ymd("2025-01-01"))
  
  # Find all previous peaks
  previous_peaks <- case_counts_yearly %>%
    group_by(location, pathogen, year) %>%
    summarise(peak_date = first(notification_date[cases == max(cases, na.rm = TRUE)]),
              peak_cases = first(cases[cases == max(cases, na.rm = TRUE)]))
  
  # Summarise the distribution by location/pathogen
  peak_summary <- previous_peaks %>%
    mutate(peak_log_cases = log(peak_cases),
           peak_day = as.numeric(peak_date - year)) %>% 
    group_by(location, pathogen) %>%
    summarise(mean_day = mean(peak_day),
              mean_log_cases = mean(peak_log_cases),
              sd_day = sd(peak_day),
              sd_log_cases = sd(peak_log_cases))
  
  # Re-sample from the summarised distribution (statistically questionable)
  peak_samples <- tibble(
    sample = 1:2000 # This must be the same number of samples as in forecast.R
  ) %>%
    expand_grid(peak_summary) %>% 
    mutate(peak_day = rnorm(n(), mean_day, sd_day),
           peak_log_cases = rnorm(n(), mean_log_cases, sd_log_cases)) %>%
    
    mutate(peak_day = pmin(365, peak_day),
           peak_day = as.integer(pmax(0, peak_day)),
           peak_cases = round(exp(peak_log_cases)))
  
  # Reformat into the necessary format
  forecast_data <-peak_samples %>%
    
    select(location, pathogen, peak_day_of_year = peak_day, peak_case_incidence = peak_cases, sample) %>%
    left_join(date_information, by = join_by(location, pathogen)) %>%
    
    mutate(
      output_type = "sample",
      output_type_id = sample
    ) %>%
    
    pivot_longer(c(peak_day_of_year, peak_case_incidence),
                 names_to = "target", values_to = "value") %>%
    
    mutate(value = as.integer(value),
           horizon = as.integer(0)) %>% 
    
    select(
      round_id, origin_date,          # Columns from date_information
      target, location, pathogen,     # Pivoted target name and identifying columns
      output_type, output_type_id,    # Columns created before pivoting
      value                           # Pivoted value column
    )
  
  
  return(forecast_data)
}


