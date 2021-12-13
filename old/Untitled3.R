# Add white line under horizontal taking bar?
# Drop skinny vertical line to taken pieces?
# Take out text, drop from top

#install.packages("rchess")
library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

source(here("create_game_dataset2.R"))

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

game_names <- list.files(pattern = ".*\\.pgn$")

games <- vector(mode = "list", length = length(game_names))
for(i in seq_along(game_names)) {
  game <- create_game_dataset(game_names[i])
  
  game_colors <- color_grids[grid == i, ]
  game <- merge(game, game_colors, by.x = c("coords"),
                by.y = c("coords"))
  
  games[[i]] <- game
}

max_turns <- max(sapply(games, function(x) max(x[, 'turn'])))

titles <- rep(c("Deep Blue (Computer) vs Garry Kasparov 1996\nGame ",
                "Garry Kasparov vs Deep Blue (Computer) 1996\nGame "), 3)
titles <- paste0(titles, seq(1, 6))

max_turns * 12
7 * 
#
#
# #
# #
# # #
# # #
# # #
# # #
# #
# #
#
#

# for(game_num in seq(1, length(games))) {
for(game_num in 1) {

  pixels <- CJ(x = seq(1, 5 * 300),
               y = seq(1, 7 * 300),
               hex_color = "#FFFFFF")
  setkey(pixels, x, y)
  
  game_pixels <- games[[game_num]]
  
  moves <- game_pixels[, .(x, turn, hex_color)]
  moves[, turn := turn + 1]
  setnames(moves, "turn", "y")
  
  ## move to correct spot
  # x = 17 needs to be center = 300 * 5 / 2 = 750
  # x = 1 needs to be left most = 750 - (12 * 16) = 558
  # y = 1 needs to be top = .25 inch down = .25 * 300 = 75 = 76
  # all needs to be 12 spaces
  
  moves[, ':=' (x = (x * 12) - 11 + 558,
                y = (y * 12) - 11 + 75)]
  setkey(moves, x, y)
  
  # unique(moves$y)
  # unique(moves$x)
  # 300 * 5 - (max(unique(moves$x)) + 13)
  
  # stretch out boxes
  moves <- moves[rep(1:.N, each = (12 * 12))][, 
                                        ':=' (x = x + rep(seq(0, 11), 12),
                                              y = y + rep(seq(0, 11), each = 12)),
                                        by = list(x, y)]
  setkey(moves, x, y)
  pixels[moves, `:=`(hex_color = i.hex_color)]
  
  ## capture square
  # similar deal
  captures_square <- game_pixels[!is.na(captured_x), .(captured_x, turn, hex_color)]
  captures_square[, turn := turn + 1]
  setnames(captures_square, c("captured_x", "turn"), c("x", "y"))
  captures_square[, ':=' (x = (x * 12) - 11 + 542,
                          y = (y * 12) - 11 + 75)]
  setkey(captures_square, x, y)
  captures_square <- captures_square[rep(1:.N, each = (8 * 8))][, 
                                              ':=' (x = x + rep(seq(2, 9), 8),
                                                    y = y + rep(seq(2, 9), each = 8)),
                                              by = list(x, y)]
  setkey(captures_square, x, y)
  pixels[captures_square, `:=`(hex_color = i.hex_color)]
  
  ## captured lines
  captures_lines <- game_pixels[!is.na(captured_x), .(captured_x, x, turn, hex_color)]
  captures_lines[, turn := turn + 1]
  captures_lines[, ':=' (x = ifelse(x < captured_x, x, captured_x),
                         x_length = abs(x - captured_x))]
  setnames(captures_lines, "turn", "y")
  captures_lines[, ':=' (x = (x * 12) - 11 + 542,
                        y = (y * 12) - 11 + 75,
                        x_length = x_length * 12)]
  setkey(captures_lines, x, y)
  
  captures_lines <- captures_lines[rep(1:.N, (x_length * 4))][, 
                                              ':=' (x = x + rep(seq(7, 7 + min(x_length) - 1), 4),
                                                    y = y + rep(seq(4, 7), each = min(x_length))),
                                              by = list(x, y)]
  setkey(captures_lines, x, y)
  pixels[captures_lines, `:=`(hex_color = i.hex_color)]
  
  ## forgot the gaps!!!!!!!!!
  gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                           24.5, 26.5, 28.5, 30.5, 31.5),
                     y = 1,
                     hex_color = "#FFFFFF")
  gaps[, ':=' (x = (x * 12) - 6 + 558,
                y = (y * 12) - 11 + 75)]
  gaps <- gaps[rep(1:.N, each = (2 * 6))][, 
                ':=' (x = x + rep(seq(0, 1), 6),
                      y = y + rep(seq(0, 5), each = 2)),
                      by = list(x, y)]
  
  setkey(gaps, x, y)
  pixels[gaps, `:=`(hex_color = i.hex_color)]
  
  
  pixels <- as.matrix(
    dcast(pixels, y ~ x, value.var = "hex_color")[, y := NULL])
  
  #open new file for output
  
  
  png(here(paste0(gsub("\\.pgn", "", game_names[game_num]), ".png")), width=300*5, height=300*7)
  par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  plot.new()
  plot.window(0:1, 0:1)
  
  #fill plot with image
  usr<-par("usr")    
  rasterImage(pixels, usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  
  #add text
  text(x = (5 - .25) / 5,
       y = (.25) / 7,
       labels = titles[[game_num]],
       adj = c(1, 0),
       cex=3,
       family = "Courier New")
  
  #close image
  dev.off()
}




