library(jsonlite)
library(readr)

end_date <- Sys.Date() - 1
start_date <- seq.Date(end_date, by = "-10 years", length.out = 2)[2]

api_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive",
  "?latitude=27.3364&longitude=-82.5307",
  "&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m",
  "&timezone=America%2FNew_York",
  "&start_date=", format(start_date, "%Y-%m-%d"),
  "&end_date=", format(end_date, "%Y-%m-%d")
)

weather <- fromJSON(api_url)
hourly <- as.data.frame(weather$hourly)
hourly$time <- as.POSIXct(
  hourly$time,
  format = "%Y-%m-%dT%H:%M",
  tz = "America/New_York"
)

write_csv(hourly, "sarasota_hourly_weather.csv")
