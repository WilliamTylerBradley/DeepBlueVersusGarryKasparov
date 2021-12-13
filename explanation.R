library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

## Piece order
piece_order <- data.table(piece = c("White King",
                                    "White Queen",
                                    "f1 Bishop",
                                    "c1 Bishop",
                                    "g1 Knight",
                                    "b1 Knight",
                                    "h1 Rook",
                                    "a1 Rook",
                                    "h2 Pawn",
                                    "a2 Pawn",
                                    "g2 Pawn",
                                    "b2 Pawn",
                                    "f2 Pawn",
                                    "c2 Pawn",
                                    "e2 Pawn",
                                    "d2 Pawn",
                                    "d7 Pawn",
                                    "e7 Pawn",                                   
                                    "c7 Pawn",
                                    "f7 Pawn",
                                    "b7 Pawn",
                                    "g7 Pawn",
                                    "a7 Pawn",
                                    "h7 Pawn",
                                    "a8 Rook",
                                    "h8 Rook",
                                    "b8 Knight",
                                    "g8 Knight",
                                    "c8 Bishop",
                                    "f8 Bishop",
                                    "Black Queen",
                                    "Black King"),
                          x = seq(1, 32))

gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                         24.5, 26.5, 28.5, 30.5, 31.5))

ggplot(data = piece_order) +
  geom_rect(aes(xmin = x - .5,
                xmax = x + .5,
                ymin = 0,
                ymax = 1),
            fill = "gray",
            color = "white" ) +
  geom_text(aes(x = x,
                y = 1,
                label = piece),
            angle = 90,
            hjust = -.1) +
  geom_rect(data = gaps,
            aes(xmin = x - .1,
                xmax = x + .1,
                ymin = .5,
                ymax = 1),
            fill = "white") +
  coord_equal() +
  lims(y = c(0, 5)) +
  ggtitle("Piece Order") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

## Colors
color_grids <- data.table(read.csv(here::here("color_grids.csv")))
color_grids[, ':=' (rank = row_num,
                    file = letters[col_num])]
color_grids[, coords := paste0(file, rank)]

ggplot(data = color_grids,
       aes(xmin = col_num - .5, 
           xmax = col_num + .5,
           ymin = row_num - .5,
           ymax = row_num + .5,
           color = hex_color, 
           fill = hex_color)) +
  geom_rect() + 
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(. ~ grid) +
  theme(legend.position = "none")

grid_size <- color_grids[grid == 1 & col_num == 1 & row_num == 8, ][, l_values] -
  color_grids[grid == 1 & col_num == 1 & row_num == 1, ][, l_values]
grid_spacing <- grid_size / 8 / 2
ggplot(data = color_grids,
       aes(xmin = (col_num - 4.5) / 8 * grid_size - grid_spacing, 
           xmax = (col_num - 4.5) / 8 * grid_size + grid_spacing,
           ymin = l_values - grid_spacing,
           ymax = l_values + grid_spacing + .1, # .1 to stop lines from showing
           color = hex_color, 
           fill = hex_color)) +
  geom_rect() + 
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(. ~ grid) +
  coord_equal() +
  theme(legend.position = "none")

color_grids <- color_grids[order(row_num)]
ggplot(data = color_grids,
       aes(h_values, c_values, 
           color = hex_color, 
           fill = hex_color,
           size = -row_num)) +
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
                     limits = c(0, 50)) +
  scale_radius(range = c(1, 10)) +
  coord_polar(start = 270 * pi / 180,
              direction = -1) +
  theme(legend.position = "none")
