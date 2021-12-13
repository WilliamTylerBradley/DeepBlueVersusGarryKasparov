# https://www.chessgames.com/index.html
# https://www.chessgames.com/perl/chessgame?gid=1070874

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
source(here("create_game_dataset.R"))

# Pull in colors
color_grids <- data.table(read.csv(here::here("color_grids.csv")))
color_grids[, ':=' (rank = row_num,
                    file = letters[col_num])]
color_grids[, coords := paste0(file, rank)]

# Pull in game names and reorder to game number
year <- 1997
game_names <- list.files(here::here(year), pattern = ".*\\.pgn$")
game_names <- game_names[c(1, 4, 2, 5, 3, 6)]

pixel_sets <- vector(mode = "list", length = length(game_names))

# Work through the games to get the images
for(game_num in seq(1, length(game_names))) {
  # print(game_num)
  ## Setup
  # Get the background color, base color with hue value rotated
  color_grid_num <- (year %% 2) * 2 + 2 * game_num - ((game_num + 1) %% 2) - 1
  
  bg_1 <- color_grids[grid == color_grid_num & col_num == 1 & row_num == 1]
  bg_2 <- color_grids[grid == color_grid_num & col_num == 8 & row_num == 8]
  
  background_color <- hcl(h = atan2(sin(bg_1$h_values * pi/180) + 
                                      sin(bg_2$h_values * pi/180),
                                    cos(bg_1$h_values * pi/180) + 
                                      cos(bg_2$h_values * pi/180)) * 180/pi + 
                            180,
                          c = (bg_1$c_values + bg_2$c_values) / 2 * (3/4),
                          l = (bg_1$l_values + bg_2$l_values) / 2)
  
  # Set up data.table for pixels, 5 x 7 / 6 at 300 dpi  
  pixels <- CJ(x = seq(1, 7 * 300 / 6),
               y = seq(1, 5 * 300),
               hex_color = background_color)
  setkey(pixels, x, y)
  
  # Pull game information
  game_pixels <- create_game_dataset(here::here(year, game_names[game_num]))
  
  game_colors <- color_grids[grid == color_grid_num, ]
  game_pixels <- merge(game_pixels, game_colors, by.x = c("coords"),
                by.y = c("coords"))
  
  ## Moves
  moves <- game_pixels[, .(x, turn, hex_color)]
  moves[, turn := turn + 1]
  setnames(moves, "turn", "y")
  
  # Move to horizontally centered and a little lower vertically
  moves[, ':=' (x = (x * 9) - 8 + 31,
                y = (y * 9) - 8 + 88)]
  setkey(moves, x, y)
  
  # Stretch out boxes to take up a 9 by 9 square of pixels
  moves <- moves[rep(1:.N, each = (9 * 9))][, 
                                        ':=' (x = x + rep(seq(0, 8), 9),
                                              y = y + rep(seq(0, 8), each = 9)),
                                        by = list(x, y)]
  setkey(moves, x, y)
  
  # Set moves in the pixels
  pixels[moves, `:=`(hex_color = i.hex_color)]
  
  ## Captured squares
  # Pull squares that were captured and set up
  captured_square <- game_pixels[!is.na(captured_x), .(captured_x, turn, hex_color)]
  captured_square[, turn := turn + 1]
  setnames(captured_square, c("captured_x", "turn"), c("x", "y"))
  captured_square[, ':=' (x = (x * 9) - 8 + 31,
                          y = (y * 9) - 8 + 88)]
  setkey(captured_square, x, y)
  
  # Stretch to 5 by 5 squares
  captured_square <- captured_square[rep(1:.N, each = (5 * 5))][, 
                                              ':=' (x = x + rep(seq(2, 6), 5),
                                                    y = y + rep(seq(2, 6), each = 5)),
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
  captured_lines[, ':=' (x = (x * 9) - 8 + 31,
                        y = (y * 9) - 8 + 88,
                        x_length = x_length * 9)]
  setkey(captured_lines, x, y)
  
  # Set line from left x value along x_length, 3 pixels thick
  # Add 4 to x to move line so it starts/ends in the middle of boxes
  captured_lines <- captured_lines[rep(1:.N, (x_length * 3))][, 
                                              ':=' (x = x + rep(seq(3, 3 + min(x_length) - 1), 3),
                                                    y = y + rep(seq(3, 5), each = min(x_length))),
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
  captured_shadows[, ':=' (x = (x * 9) - 8 + 31,
                         y = (y * 9) - 8 + 88 + 5,
                         x_length = x_length * 9)]
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
  gaps[, ':=' (x = (x * 9) - 9 / 2 + 31,
                y = (y * 9) - 8 + 88)]
  gaps <- gaps[rep(1:.N, each = (2 * 5))][, 
                                          ':=' (x = x + rep(seq(0, 1), 5),
                                                y = y + rep(seq(0, 4), each = 2)),
                                          by = list(x, y)]
  setkey(gaps, x, y)
  pixels[gaps, `:=`(hex_color = i.hex_color)]
  
  # move to the left for each game
  pixels[, x := x + (game_num - 1) * (300 * 7 / 6)]
  
  pixel_sets[[game_num]] <- pixels
  
}

pixels <- rbindlist(pixel_sets)

## Convert pixels to matrix
#  convert pixels from long to wide
pixels <- as.matrix(
  dcast(pixels, y ~ x, value.var = "hex_color")[, y := NULL])

# Open new file for output
png(here::here(year, paste0("DeepBlueVersusGarryKasparov", year, ".png")), 
    width = 300 * 7, 
    height = 300 * 5)
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
