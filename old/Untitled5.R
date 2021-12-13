# This file creates the grids of colors
library(colorspace)
library(data.table)
library(ggplot2)
library(tidyverse)

hue <- seq(0, 359.5, .5)
luminance <- seq(0, 100, .5)

max_chroma_luminance <- function(h) {
  mc <- max_chroma(h, l = seq(0, 100, .5))
  ml <- seq(0, 100, .5)[mc == max(mc)]
  data.frame(hue = h,
             chroma = max(mc),
             luminance = ml)
}

df <- map_dfr(hue, max_chroma_luminance)

df <- df %>%
  mutate(x = chroma * cos(hue * pi/180),
         y = chroma * sin(hue * pi/180),
         hexcolor = hcl(hue, chroma, luminance))

ggplot(data = df,
       aes(hue, chroma, 
           color = hexcolor, 
           fill = hexcolor)) +
  geom_point() + 
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous("Hue",
                     limits = c(0, 360),
                     breaks = seq(45, 360, 45),
                     minor_breaks = seq(0, 315, 45) + 45/2,
                     labels = c('45', '90', '135', '180', 
                                '225', '270', '315', '0|360')) +
  scale_y_continuous("Chroma",
                     limits = c(0, 180)) +
  coord_polar(start = 270 * pi / 180,
              direction = -1) +
  theme(legend.position = "none")

ggplot(data = df,
       aes(hue, chroma, 
           color = hexcolor, 
           fill = hexcolor)) +
  geom_point() + 
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous("Hue",
                     limits = c(0, 360),
                     breaks = seq(0, 360, 60),
                     minor_breaks = seq(0, 360, 60) + 30) +
  scale_y_continuous("Chroma",
                     limits = c(0, 180)) +
  coord_polar(start = 270 * pi / 180,
              direction = -1) +
  theme(legend.position = "none")

ggplot(data = df,
       aes(hue, chroma, 
           color = hexcolor, 
           fill = hexcolor)) +
  geom_point() + 
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous("Hue",
                     limits = c(0, 360),
                     breaks = seq(0, 360, 60) + 30,
                     minor_breaks = seq(0, 360, 30)) +
  scale_y_continuous("Chroma",
                     limits = c(0, 180)) +
  theme(legend.position = "none")


# bi color scale?
# 1 red, yellow - orange
# 2 orange, green - yellow
# 3 orange, blue - green
# 4
# 5
# 6

# Keep the edges of the hue values locked, 
# Then figure out how far I can push the grid out
# I think we can use optimization for this?
# Need chroma 1, chroma 2, and luminance values.




