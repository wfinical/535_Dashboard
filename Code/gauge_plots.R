forest_area <- read_csv("Data/forest_area.csv")
library(patchwork)

usa <- forest_area %>%
  filter(code == "USA") %>%
  mutate(pct_change = (forest_area/lag(forest_area) - 1) * 100)
usa$forest_area <- round(usa$forest_area, 1)

bra <- forest_area %>%
  filter(code == "BRA") %>%
  mutate(pct_change = (forest_area/lag(forest_area) - 1) * 100)
bra$forest_area <- round(bra$forest_area, 1)

chn <- forest_area %>%
  filter(code == "CHN") %>%
  mutate(pct_change = (forest_area/lag(forest_area) - 1) * 100)
chn$forest_area <- round(chn$forest_area, 1)

#### BRAZIL

brazil_plot <- bra %>%
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = ~forest_area,
    title = list(text = "Brazil Forest Area 1990-2020"),
    frame = ~year,
    type = "indicator",
    mode = "gauge+number",
    gauge = list(axis =list(range = list(NULL, 20)),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = 13.9)))

brazil_plot <- brazil_plot %>%
  layout(margin = list(l=20,r=30))

brazil_plot

#### CHINA

china_plot <- chn %>%
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = ~forest_area,
    title = list(text = "China Forest Area 1990-2020"),
    frame = ~year,
    type = "indicator",
    mode = "gauge+number",
    gauge = list(axis =list(range = list(NULL, 20)),
                 threshold = list(
                   line = list(color = "red", width = 4),
                   thickness = 0.75,
                   value = 3.7)))

china_plot <- china_plot %>%
  layout(margin = list(l=20,r=30))

china_plot

#### USA

usa_plot <- usa %>%
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = ~forest_area,
    title = list(text = "United States Forest Area 1990-2020"),
    frame = ~year,
    type = "indicator",
    mode = "gauge+number",
    gauge = list(axis =list(range = list(NULL, 20)),
                 threshold = list(
                   line = list(color = "red", width = 4),
                   thickness = 0.75,
                   value = 7.1)))

usa_plot <- usa_plot %>%
  layout(margin = list(l=20,r=30))

usa_plot

