### User input -----------------------------------------------------------------
corr <- 0.4

## Colors
# low_color <- "#bfff00"#lime
low_color <- "#FCFCFC"

# high_color <- "#FF0000" # red
# high_color <- "#00bfff" # blue
high_color <- "#bcbcbc" # gray

### Create logo ----------------------------------------------------------------
library(tidyverse)
library(hexSticker)
library(png)
library(grid)
library(showtext)
font_add_google(name = "Roboto", family = "Roboto")

showtext_auto()


l <- 1

hex_poly <- data_frame(
  x = 1.35 * l * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0, rep(-sqrt(3) / 2, 2)),
  y = 1.35 * l * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5)
)

crossing(x = seq(min(hex_poly$x), max(hex_poly$x), 0.01),
         y = seq(min(hex_poly$y), max(hex_poly$y), 0.01)
) %>%
  mutate(
    location = sp::point.in.polygon(point.x = x,
                                    point.y = y,
                                    pol.x = hex_poly$x,
                                    pol.y = hex_poly$y)
  ) %>%
  filter(location == 1) %>%
  select(-location) %>%
  mutate_if(is.double, round, digits = 2) -> hex_points

means <- c(median(hex_points$x), median(hex_points$y))
xvar <- ((((max(hex_points$x) - min(hex_points$x)) / 2) / 3)^2) * 1.5
yvar <- ((((max(hex_points$y) - min(hex_points$y)) / 2) / 3)^2) * 1.5
cor_mat <- matrix(data = c(1, corr, corr, 1), ncol = 2, byrow = TRUE)
stdevs <- c(sqrt(xvar), sqrt(yvar))
b <- stdevs %*% t(stdevs)
sigma <- b * cor_mat

gen_data <- mvtnorm::rmvnorm(1000000, mean = means, sigma = sigma) %>%
  as_tibble(.name_repair = "universal") %>%
  mutate_if(is.double, round, digits = 2) %>%
  count(...1, ...2)

hex_points <- left_join(hex_points, gen_data, by = c("x" = "...1", "y" = "...2")) %>%
  replace_na(list(n = 0))

theme_hex <- theme_void() + theme_transparent() +
  theme(axis.ticks.length = unit(0, "mm"))

img <- readPNG("~/CamaraBR/inst/doc/camara.png")
g <- rasterGrob(img, interpolate=TRUE)

flag <- readPNG("~/CamaraBR/inst/doc/bandeira.png")
b <- rasterGrob(flag, interpolate=TRUE)

plot_sample <- hex_points %>%
  uncount(weights = n) %>%
  sample_n(size = 125)

ggplot() +
  geom_raster(data = hex_points, aes(x = x, y = y, fill = n),
    show.legend = FALSE) +
  geom_polygon(data = hex_poly, aes(x, y), color = high_color, alpha = 0,
    size = 22) +
  annotate("text", x = 0, y = 0.66, label= "CamaraBR",
           size = 44, family = "Roboto", fontface="bold") + 
  annotation_custom(g, xmin = -1.05, xmax = 1.05, ymin = -1.15, ymax = 1.0) +
  annotation_custom(b, xmin = -0.25, xmax = 0.22, ymin = -1.35, ymax = 0.40) +
  coord_equal(xlim = range(hex_poly$x), ylim = range(hex_poly$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0.04, 0)) +
  scale_fill_gradient(low = low_color, high = high_color) +
  theme_hex -> logo


png("~/CamaraBR/inst/doc/CamaraBR-logo.png", width = 905, height = 1050, bg = "transparent")
print(logo)
dev.off()

ggplot() +
  geom_raster(data = hex_points, aes(x = x, y = y, fill = n),
              show.legend = FALSE) +
  geom_polygon(data = hex_poly, aes(x, y), color = high_color, alpha = 0,
               size =  0.35) +
  annotate("text", x = 0, y = 0.66, label= "CamaraBR",
           size = 1.3, family = "Roboto", fontface="bold") + 
  annotation_custom(g, xmin = -1.05, xmax = 1.05, ymin = -1.15, ymax = 1.0) +
  annotation_custom(b, xmin = -0.25, xmax = 0.22, ymin = -1.25, ymax = 0.40) +
  coord_equal(xlim = range(hex_poly$x), ylim = range(hex_poly$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0.04, 0)) +
  scale_fill_gradient(low = low_color, high = high_color) +
  theme_hex ->  icon

png("~/CamaraBR/inst/doc/CamaraBR-icon.png", width = 32, height = 32, bg = "transparent")
print(icon)
dev.off()


ggplot() +
  geom_raster(data = hex_points, aes(x = x, y = y, fill = n),
              show.legend = FALSE) +
  geom_polygon(data = hex_poly, aes(x, y), color = high_color, alpha = 0,
               size =  2.12) +
  annotate("text", x = 0, y = 0.66, label= "CamaraBR",
           size = 8, family = "Roboto", fontface="bold") + 
  annotation_custom(g, xmin = -1.05, xmax = 1.05, ymin = -1.15, ymax = 1.0) +
  annotation_custom(b, xmin = -0.25, xmax = 0.22, ymin = -1.25, ymax = 0.40) +
  coord_equal(xlim = range(hex_poly$x), ylim = range(hex_poly$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0.04, 0)) +
  scale_fill_gradient(low = low_color, high = high_color) +
  theme_hex ->  icon_192

png("~/CamaraBR/inst/doc/CamaraBR-icon-192.png", width = 192, height = 192, bg = "transparent")
print(icon_192)
dev.off()
