# Validating results against Source Aportionment studies



m.dew <- creadeweather::deweather(
  city="Beijing",
  poll=rcrea::PM25,
  source="mee",
  add_fire = T,
  upload_results = F,
  output=c("anomaly","anomaly_yday"),
  fire_mode = "trajectory",
  training_start_anomaly="2017-01-01",
  training_end_anomaly="2021-12-31",
  lag=c(0,1,2,3),
  training.fraction=c(0.8, 0.9, 1),
  save_weather_filename="cache/weather.beijing.RDS",
  # read_weather_filename=file.cache.weather,
  keep_model = T,
  link=c("linear"),
  upload_results = F
)