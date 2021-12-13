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
game_names <- game_names[c(1, 4, 2, 5, 3, 6)]

games <- vector(mode = "list", length = length(game_names))
for(i in seq_along(game_names)) {
  game <- create_game_dataset(game_names[i])
  
  game_colors <- color_grids[grid == i, ]
  game <- merge(game, game_colors, by.x = c("coords"),
                by.y = c("coords"))
  
  games[[i]] <- game
}

max_turns <- max(sapply(games, function(x) max(x[, 'turn'])))

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



for(game_num in seq(1, length(games))) {
#for(game_num in 1) {
  background_color <- hcl(color_grids[color_grids$grid == game_num, 
                                      ][1, "base_h"] + 180,
                          color_grids[color_grids$grid == game_num, 
                                      ][1, "base_c"],
                          color_grids[color_grids$grid == game_num, 
                                      ][1, "base_l"])
  
  # background_color <- hcl(color_grids[color_grids$grid == game_num, 
  # ][1, "base_h"] + 180,
  # color_grids[color_grids$grid == game_num, 
  # ][1, "base_c"] / 4,
  # max(color_grids[color_grids$grid == game_num, "l_values"]))
   
  pixels <- CJ(x = seq(1, 5 * 300),
               y = seq(1, 7 * 300),
               hex_color = background_color)
  setkey(pixels, x, y)
  
  game_pixels <- games[[game_num]]
  
  moves <- game_pixels[, .(x, turn, hex_color)]
  moves[, turn := turn + 1]
  setnames(moves, "turn", "y")
  
  ## move to correct spot
  # x = 17 needs to be center = 300 * 5 / 2 = 750
  # x = 1 needs to be left most = 750 - (12 * 16) = 558
  # y = 1 needs to be top = .5 inch down = .5 * 300 = 150 = 151
  # all needs to be 12 spaces
  # What if we just add this at the end? - yes
  
  moves[, ':=' (x = (x * 12) - 11 + 558,
                y = (y * 12) - 11 + 151)]
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
  captures_square <- game_pixels[!is.na(captured_x), .(captured_x, turn, hex_color)]
  captures_square[, turn := turn + 1]
  setnames(captures_square, c("captured_x", "turn"), c("x", "y"))
  captures_square[, ':=' (x = (x * 12) - 11 + 558,
                          y = (y * 12) - 11 + 151)]
  setkey(captures_square, x, y)
  captures_square <- captures_square[rep(1:.N, each = (8 * 8))][, 
                                              ':=' (x = x + rep(seq(2, 9), 8),
                                                    y = y + rep(seq(2, 9), each = 8)),
                                              by = list(x, y)]
  setkey(captures_square, x, y)
  pixels[captures_square, `:=`(hex_color = i.hex_color)]
  
  # Add captured lines
  captures_lines <- game_pixels[!is.na(captured_x), .(captured_x, x, turn, hex_color)]
  captures_lines[, turn := turn + 1]
  captures_lines[, ':=' (x = ifelse(x < captured_x, x, captured_x),
                         x_length = abs(x - captured_x))]
  setnames(captures_lines, "turn", "y")
  captures_lines[, ':=' (x = (x * 12) - 11 + 558,
                        y = (y * 12) - 11 + 151,
                        x_length = x_length * 12)]
  setkey(captures_lines, x, y)
  
  captures_lines <- captures_lines[rep(1:.N, (x_length * 4))][, 
                                              ':=' (x = x + rep(seq(7, 7 + min(x_length) - 1), 4),
                                                    y = y + rep(seq(4, 7), each = min(x_length))),
                                              by = list(x, y)]
  setkey(captures_lines, x, y)
  pixels[captures_lines, `:=`(hex_color = i.hex_color)]
  
  # Add captured lines shadows
  captures_shadows <- game_pixels[!is.na(captured_x), .(captured_x, x, turn)]
  captures_shadows[, ':=' (turn = turn + 1,
                           hex_color = background_color)]
  captures_shadows[, ':=' (x = ifelse(x < captured_x, x + 1, captured_x + 1),
                         x_length = abs(x - captured_x) - 1)]
  setnames(captures_shadows, "turn", "y")
  captures_shadows[, ':=' (x = (x * 12) - 11 + 558,
                         y = (y * 12) - 11 + 151 + 7,
                         x_length = x_length * 12)]
  setkey(captures_shadows, x, y)
  
  captures_shadows <- captures_shadows[rep(1:.N, (x_length))][, 
                                                              ':=' (x = x + seq(0, min(x_length) - 1),
                                                                    y = y + rep(1, each = min(x_length))),
                                                              by = list(x, y)]
  setkey(captures_shadows, x, y)
  pixels[captures_shadows, `:=`(hex_color = i.hex_color)]
  
  # Add gaps
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

  #close image
  dev.off()
}




