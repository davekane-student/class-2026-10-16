library(ggplot2)
library(readr)

weather <- read_csv("sarasota_hourly_weather.csv", show_col_types = FALSE)
weather$time <- as.POSIXct(
  weather$time,
  format = "%Y-%m-%dT%H:%M",
  tz = "America/New_York"
)

weather$interval_start <- weather$time[(seq_len(nrow(weather)) - 1L) %/% 2L * 2L + 1L]
two_hour <- aggregate(precipitation ~ interval_start, data = weather, FUN = sum)

storms <- data.frame(
  name = c(
    "Irma (2017)", "Michael (2018)", "Ian (2022)",
    "Idalia (2023)", "Helene (2024)", "Milton (2024)"
  ),
  target_time = as.POSIXct(
    c("2017-09-10", "2018-10-10", "2022-09-28",
      "2023-08-30", "2024-09-26", "2024-10-09"),
    format = "%Y-%m-%d",
    tz = "America/New_York"
  ),
  label_time = as.POSIXct(
    c("2017-03-01", "2019-02-01", "2021-01-01",
      "2022-11-01", "2024-01-01", "2025-08-01"),
    format = "%Y-%m-%d",
    tz = "America/New_York"
  )
)

storms$target_precipitation <- NA_real_
for (i in seq_len(nrow(storms))) {
  day_bins <- which(as.Date(two_hour$interval_start) == as.Date(storms$target_time[i]))
  target_bin <- day_bins[which.max(two_hour$precipitation[day_bins])]
  storms$target_time[i] <- two_hour$interval_start[target_bin]
  storms$target_precipitation[i] <- two_hour$precipitation[target_bin]
}
storms$label_height <- max(two_hour$precipitation, na.rm = TRUE) * 1.12

plot <- ggplot(two_hour, aes(x = interval_start, y = precipitation)) +
  geom_col(width = 2 * 60 * 60, fill = "#243b53", color = "#243b53") +
  geom_segment(
    data = storms,
    aes(
      x = label_time,
      y = label_height - 2,
      xend = target_time,
      yend = target_precipitation
    ),
    inherit.aes = FALSE,
    color = "#c2410c",
    linewidth = 0.6,
    arrow = grid::arrow(length = grid::unit(0.12, "inches"))
  ) +
  geom_label(
    data = storms,
    aes(x = label_time, y = label_height, label = name),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold",
    color = "#7c2d12",
    fill = "#fff7ed",
    linewidth = 0.3
  ) +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y",
    timezone = "America/New_York"
  ) +
  labs(
    title = "Ten Years of Sarasota Rainfall",
    subtitle = "Precipitation totals in consecutive two-hour intervals",
    x = NULL,
    y = "Precipitation (mm)",
    caption = "Source: Open-Meteo | Sarasota, Florida"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e5e7eb"),
    axis.text = element_text(color = "#374151", size = 13),
    axis.title = element_text(color = "#1f2937", size = 16),
    plot.title = element_text(color = "#111827", face = "bold", size = 22),
    plot.subtitle = element_text(color = "#4b5563"),
    plot.caption = element_text(color = "#6b7280")
  )

ggsave("sarasota-weather.png", plot, width = 14, height = 6, dpi = 180)
