library(tidyverse)
library(jardskjalftar)
library(sf)
library(slider)
library(metill)
library(glue)
library(patchwork)
library(ggforce)

theme_set(theme_metill())
end_time <- Sys.time()
start_time <- clock::date_time_build(
  2021,
  1,
  1,
  0,
  0,
  1,
  zone = "GMT"
)

skjalftalisa_data <- download_skjalftalisa_data(start_time, end_time)

areas_url <- "https://api.vedur.is/skjalftalisa/v1/areas"

res <- httr2::request(areas_url) |>
  httr2::req_headers(
    accept = "application/json"
  ) |>
  httr2::req_perform()

areas <- dplyr::tibble(data = httr2::resp_body_json(res)) |>
  tidyr::unnest_wider(data) |>
  tidyr::unnest_wider(area_json) |>
  tidyr::unnest_longer(polygon) |>
  tidyr::unnest_wider(polygon, names_sep = "_") |>
  sf::st_as_sf(coords = c("polygon_2", "polygon_1"), crs = "WGS84") |>
  dplyr::group_by(name) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")


get_areas <- function(area) {
  out <- areas |>
    dplyr::filter(
      name == area
    )

  out$geometry[[1]][[1]] |>
    t() |>
    as.numeric()
}
reykj <- areas |>
  dplyr::filter(name == "Reykjanesskagi - VÍ")

reykj_area <- reykj$geometry[[1]][[1]] |>
  t() |>
  as.numeric()

jardskjalftar:::build_skjalftalisa_query(
  start_time = Sys.time() - lubridate::days(1),
  end_time = Sys.time(),
  area = get_areas("Hengillinn - VÍ")
) |>
  jsonlite::toJSON(
    auto_unbox = TRUE,
    pretty = TRUE
  )


chosen_area <- "Reykjanesskagi - VÍ"

p <- skjalftalisa_data |>
  sf::st_join(
    areas |>
      dplyr::filter(
        name == "Reykjanesskagi - VÍ"
      ),
    join = sf::st_within,
    left = FALSE
  ) |>
  st_drop_geometry() |>
  arrange(time) |>
  mutate(
    date = as_date(time),
    value = exp((magnitude + 6) * 3 / 2),
    value = slide_index_dbl(value, time, sum, .before = days(1))
  ) |>
  ggplot(aes(time, value)) +
  geom_line() +
  geom_vline(
    data = tibble(
      date = clock::date_time_build(
        c(2021, 2022, 2023, 2023, 2024),
        c(3, 8, 7, 12, 1),
        c(19, 3, 10, 18, 14),
        c(20, 1, 1, 22, 8),
        c(45, 1, 1, 17, 0),
        zone = "GMT"
      )
    ),
    aes(
      xintercept = as_datetime(date)
    ),
    lty = 2,
    col = "red",
    alpha = 0.5
  ) +
  scale_x_datetime(
    expand = expansion(),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Skjálftavægi og eldgos",
    subtitle = "Sólarhrings heildarsumma skjálftavægis. Upphaf eldgosa með rauðum brotnum línum.",
    caption = str_c(
      "Vægi er reiknað út frá stærð miðað við útskýringar Vísindavefs: ",
      "https://www.visindavefur.is/svar.php?id=81389"
    )
  )


p


ggsave(
  plot = p,
  filename = "Figures/skjalftavaegi.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)





