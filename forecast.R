

library(tidyverse)
library(mgcv)
library(gratia)

# Read in the data we are going to forecast (flu case counts from Victoria)
flu_data <- read_csv("...")

# Filter for just this season
flu_data_recent <- flu_data %>%
  filter(notification_date >= ymd("2024-03-01"))

# Read and extract the relevant date information
date_information <- read_csv("...") %>%
  filter(pathogen == "flu")

origin_date <- date_information$origin_date
forecast_date <- date_information$forecast_date

# Plot out data
ggplot() +
  geom_line(aes(x = notification_date, y = cases),
            flu_data_recent) +
  
  geom_vline(xintercept = origin_date, colour = ggokabeito::palette_okabe_ito(5)) +
  
  theme_bw()

# Modify our data to make it readable by mgcv
model_data <- flu_data_recent %>%
  filter(notification_date >= max(notification_date) - days(14)) %>% 
  mutate(t = as.numeric(notification_date - min(notification_date)),
         day_of_week = wday(notification_date),
         log_count = log(cases))


gam_fit <- mgcv::gam(log_count ~ t + s(day_of_week, k = 3), data = model_data)

# Create a tibble with the future dates in the
# same format as model_data
forecasting_predictor_data <- tibble(
  notification_date = seq(origin_date, origin_date + days(28), by = "days")
) %>%
  
  mutate(t = as.numeric(notification_date - min(model_data$notification_date)),
         day_of_week = wday(notification_date),
         .row = row_number())


# Produce our predictions across the posterior of the gam
forecasting_predictions <- gratia::posterior_samples(
  gam_fit,
  n = 2000,
  data = forecasting_predictor_data
) %>%
  left_join(forecasting_predictor_data) %>%
  
  mutate(pred_count = round(exp(.response)))
  


# Plot our predictions
ggplot() +
  
  geom_line(aes(x = notification_date, y = cases),
            flu_data_recent) +
  
  geom_line(aes(x = notification_date, y = pred_count, group = .draw),
            alpha = 0.5, colour = ggokabeito::palette_okabe_ito(5),
            forecasting_predictions %>% filter(.draw < 5)) +
  
  geom_vline(xintercept = origin_date, colour = ggokabeito::palette_okabe_ito(5)) +
  
  scale_fill_brewer() +
  
  scale_y_continuous(limits = c(0, 900)) +
  
  theme_bw()


ggplot() +
  
  ggdist::stat_ribbon(
    aes(x = notification_date, y = pred_count),
    forecasting_predictions
  ) +
  geom_line(aes(x = notification_date, y = cases),
            flu_data_recent) +
  
  geom_vline(xintercept = origin_date, colour = ggokabeito::palette_okabe_ito(5)) +
  
  scale_fill_brewer() +
  
  scale_y_continuous(limits = c(0, 900)) +
  
  theme_bw()


# Reformat out predictions to match the necessary table
# format required by the hub
forecast_data <- forecasting_predictions %>%
  
  mutate(
    pathogen = "flu",
    location = "VIC",
    horizon = as.integer(notification_date - origin_date),
    target = "case incidence", 
    origin_date = origin_date,
    forecast_date = "2024-09-20",
    output_type = "sample",
    output_type_id = .draw,
    value = as.integer(pred_count)
  ) %>%
  
  select(
    forecast_date, origin_date,
    target, horizon, location, pathogen,
    
    output_type, output_type_id,
    
    value
  )

forecast_data

# Make sure our file name matches forecast_date and our model name
forecast_file_name <- str_c(forecast_date, "-uom-testing.parquet")

# Write out our forecast_data as a parquet file
arrow::write_parquet(forecast_data, forecast_file_name)


