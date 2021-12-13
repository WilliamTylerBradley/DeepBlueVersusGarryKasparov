# Wrap all into one image? 4 X 6?
# 384 * 1764
1764 / (384 * 6)

1800 / (420 * 6)

5 / 7

5 * 300

#
#
# #
# #
# # #
# # #
# # #
# #
# #
#
#

## This file pulls in the chess games and colors, sets up the data 
#  structures, and creates the images.

#install.packages("rchess")
library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

# Pull in code to clean up chess data
source(here("create_game_dataset2.R"))

# Pull in colors
color_grids <- data.table(read.csv(here::here("color_grids.csv")))
color_grids[, ':=' (rank = row_num,
                    file = letters[col_num])]
color_grids[, coords := paste0(file, rank)]

# Pull in game names and reorder to game number
game_names <- list.files(pattern = ".*\\.pgn$")
game_names <- game_names[c(1, 4, 2, 5, 3, 6)]

# Work through the games to get the images
for(game_num in seq(1, length(game_names))) {
  print(game_num)
  ## Setup
  # Get the background color, base color with hue value rotated
  # background_color <- hcl(color_grids[color_grids$grid == game_num, 
  #                                     ][1, "base_h"] + 180,
  #                         color_grids[color_grids$grid == game_num, 
  #                                     ][1, "base_c"] * (3/4),
  #                         color_grids[color_grids$grid == game_num, 
  #                                     ][1, "base_l"])

  bg_1 <- color_grids[grid == game_num & col_num == 1 & row_num == 1]
  bg_2 <- color_grids[grid == game_num & col_num == 8 & row_num == 8]
  
  background_color <- hcl(h = (bg_1$h_values + bg_2$h_values) / 2 + 180,
                          c = (bg_1$c_values + bg_2$c_values) / 2 * (3/4),
                          l = (bg_1$l_values + bg_2$l_values) / 2)
  
  # Set up data.table for pixels, 5 x 7 at 300 dpi  
  pixels <- CJ(x = seq(1, 5 * 300),
               y = seq(1, 7 * 300),
               hex_color = background_color)
  setkey(pixels, x, y)
  
  # Pull game information
  game_pixels <- create_game_dataset(game_names[game_num])
  
  game_colors <- color_grids[grid == game_num, ]
  game_pixels <- merge(game_pixels, game_colors, by.x = c("coords"),
                by.y = c("coords"))
  
  ## Moves
  moves <- game_pixels[, .(x, turn, hex_color)]
  moves[, turn := turn + 1]
  setnames(moves, "turn", "y")
  
  # Move to horizontally centered and a little lower vertically
  # x = 17 needs to be center = 300 * 5 / 2 = 750
  # x = 1 needs to be left most = 750 - (12 * 16) = 558
  # y = 1 needs to be top = .5 inch down = .5 * 300 = 150 = 151
  # all needs to be 12 spaces
  moves[, ':=' (x = (x * 12) - 11 + 558,
                y = (y * 12) - 11 + 151)]
  setkey(moves, x, y)
  
  # Stretch out boxes to take up a 12 by 12 square of pixels
  moves <- moves[rep(1:.N, each = (12 * 12))][, 
                                        ':=' (x = x + rep(seq(0, 11), 12),
                                              y = y + rep(seq(0, 11), each = 12)),
                                        by = list(x, y)]
  setkey(moves, x, y)
  
  # Set moves in the pixels
  pixels[moves, `:=`(hex_color = i.hex_color)]
  
  ## Captured squares
  # Pull squares that were captured and set up
  captured_square <- game_pixels[!is.na(captured_x), .(captured_x, turn, hex_color)]
  captured_square[, turn := turn + 1]
  setnames(captured_square, c("captured_x", "turn"), c("x", "y"))
  captured_square[, ':=' (x = (x * 12) - 11 + 558,
                          y = (y * 12) - 11 + 151)]
  setkey(captured_square, x, y)
  
  # Stretch to 8 by 8 squares
  captured_square <- captured_square[rep(1:.N, each = (8 * 8))][, 
                                              ':=' (x = x + rep(seq(2, 9), 8),
                                                    y = y + rep(seq(2, 9), each = 8)),
                                              by = list(x, y)]
  setkey(captured_square, x, y)
  
  # Set captured squares in the pixels
  pixels[captured_square, `:=`(hex_color = i.hex_color)]
  
  # Captured lines
  # Pull squares that were captured and set up
  captured_lines <- game_pixels[!is.na(captured_x), .(captured_x, x, turn, hex_color)]
  captured_lines[, turn := turn + 1]
  
  # Set everything to be left to right
  captured_lines[, ':=' (x = ifelse(x < captured_x, x, captured_x),
                         x_length = abs(x - captured_x))]
  
  # Continue to set up
  setnames(captured_lines, "turn", "y")
  captured_lines[, ':=' (x = (x * 12) - 11 + 558,
                        y = (y * 12) - 11 + 151,
                        x_length = x_length * 12)]
  setkey(captured_lines, x, y)
  
  # Set line from left x value along x_length, 4 pixels thick
  # Add 7 to x to move line so it starts/ends in the middle of boxes
  captured_lines <- captured_lines[rep(1:.N, (x_length * 4))][, 
                                              ':=' (x = x + rep(seq(7, 7 + min(x_length) - 1), 4),
                                                    y = y + rep(seq(4, 7), each = min(x_length))),
                                              by = list(x, y)]
  setkey(captured_lines, x, y)
  pixels[captured_lines, `:=`(hex_color = i.hex_color)]
  
  ## Captured lines shadows
  captured_shadows <- game_pixels[!is.na(captured_x), .(captured_x, x, turn)]
  
  # Use background_color
  captured_shadows[, ':=' (turn = turn + 1,
                           hex_color = background_color)]
  
  # Shrink size to not overlap the capturing/captured squares
  captured_shadows[, ':=' (x = ifelse(x < captured_x, x + 1, captured_x + 1),
                         x_length = abs(x - captured_x) - 1)]
  setnames(captured_shadows, "turn", "y")
  captured_shadows[, ':=' (x = (x * 12) - 11 + 558,
                         y = (y * 12) - 11 + 151 + 7,
                         x_length = x_length * 12)]
  setkey(captured_shadows, x, y)
  
  captured_shadows <- captured_shadows[rep(1:.N, x_length)][, 
                                                              ':=' (x = x + seq(0, min(x_length) - 1),
                                                                    y = y + rep(1, each = min(x_length))),
                                                              by = list(x, y)]
  setkey(captured_shadows, x, y)
  pixels[captured_shadows, `:=`(hex_color = i.hex_color)]
  
  ## Gaps at the top to separate out piece sections
  gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                           24.5, 26.5, 28.5, 30.5, 31.5),
                     y = 1,
                     hex_color = background_color)
  gaps[, ':=' (x = (x * 12) - 6 + 558,
                y = (y * 12) - 11 + 151)]
  gaps <- gaps[rep(1:.N, each = (2 * 6))][, 
                ':=' (x = x + rep(seq(0, 1), 6),
                      y = y + rep(seq(0, 5), each = 2)),
                      by = list(x, y)]
  setkey(gaps, x, y)
  pixels[gaps, `:=`(hex_color = i.hex_color)]
  
  ## Convert pixels to matrix
  #  convert pixels from long to wide
  pixels <- as.matrix(
    dcast(pixels, y ~ x, value.var = "hex_color")[, y := NULL])
  
  # Open new file for output
  png(here(paste0(gsub("\\.pgn", "", game_names[game_num]), ".png")), 
      width = 300 * 5, 
      height = 300 * 7)
  par(mar = c(0, 0, 0, 0), 
      xpd = NA, 
      mgp = c(0, 0, 0), 
      oma = c(0, 0, 0, 0),
      ann = F)
  plot.new()
  plot.window(0:1, 0:1)
  
  #fill plot with image
  usr <- par("usr")    
  rasterImage(pixels, usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)

  #close image
  dev.off()
}
