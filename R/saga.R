library(tidyverse)
library(jardskjalftar)
library(sf)
library(slider)
library(metill)
library(glue)
library(patchwork)

theme_set(theme_metill())
end_time <- Sys.time()
start_time <- end_time - lubridate::years(7)

skjalftalisa_data <- download_skjalftalisa_data(start_time, end_time)


when_eldgos <- tibble(
  date = clock::date_build(
    c(2014, 2021, 2022, 2023, 2023, 2024),
    c(8, 3, 8, 7, 12, 1),
    c(29, 16, 3, 10, 18, 14)
  )
) |>
  filter(
    date >= Sys.Date() - lubridate::years(7)
  )

p1 <- skjalftalisa_data |>
  st_drop_geometry() |>
  arrange(time) |>
  mutate(
    date = as_date(time)
  ) |>
  crossing(
    magn_filt = 0:4
  ) |>
  mutate(
    larger = 1 * (magnitude >= magn_filt)
  ) |>
  summarise(
    n = sum(larger),
    .by = c(date, magn_filt)
  ) |>
  mutate(
    roll_n = slide_dbl(n, sum, .before = 7),
    .by = magn_filt
  ) |>
  mutate(
    magn_filt = if_else(
      magn_filt == 0, "Fjöldi alls",
      glue("Fjöldi jarðskjálfta af stærð > {magn_filt}")
    )
  ) |>
  ggplot(aes(date, roll_n)) +
  geom_area() +
  geom_vline(
    data = when_eldgos,
    aes(
      xintercept = date
    ),
    lty = 2,
    col = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_wrap(
    "magn_filt",
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Jarðskjálftar eftir stærð og eldgos á Íslandi (2017 - 2024)",
    subtitle = "Upphaf eldgosa með rauðum línum"
  )

p1

ggsave(
  plot = p1,
  filename = "Figures/saga_vika.png",
  width = 8, height = 1.3 * 8, scale = 1
)


