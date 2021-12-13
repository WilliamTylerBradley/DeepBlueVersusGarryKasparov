#install.packages("rchess")
library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

source(here("create_game_dataset.R"))

game_names <- list.files(pattern = ".*\\.pgn$")

hue_turns <- (seq(1, 360, length.out = 7)[1:6] + 1996) %% 360
# ggplot(data = data.frame(x = cos(hue_turns * pi/180),
#                          y = sin(hue_turns * pi/180))) +
#   geom_point(aes(x = x,
#                  y = y)) +
#   coord_equal()

games <- vector(mode = "list", length = length(game_names))
turns <- vector(mode = "list", length = length(game_names))
for(i in seq_along(game_names)) {
  games[[i]] <- create_game_dataset(game_names[i], hue_turns[i])
  turns[[i]] <- max(games[[i]][, 'turn'])
}

max_turns <- max(as.numeric(turns))

titles <- rep(c("Deep Blue (Computer) vs Garry Kasparov 1996\nGame ",
                "Garry Kasparov vs Deep Blue (Computer) 1996\nGame "), 3)
titles <- paste0(titles, seq(1, 6))

gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                         24.5, 26.5, 28.5, 30.5, 31.5))

## ok so, 5 width x 8 height
## take off .5 inch all the way around
## so 4 x 7
## take off quarter inch for text?
## so 4 x 6.75
## max turns is 146 needs to fit in 6.75 inch
## 6.75 / 146 
## floor(6.75 / 146 * 100) / 100 is side of square = .04
## turns go from 1:146 to 

df <- CJ(x = seq(1, 5 * 300),
         y = seq(8 * 300))


library(ragg)
library(grid)
square_size <- 13/300
agg_png(here("test.png"), width = 5, height = 8, units = 'in', res = 300)

for(x in 1:32) {
  for(turns in 1:146) {
    grid.rect(x = (2.5 + (x - 16) * square_size),
              y = 7.5 - (max_turns - turns) * square_size,
              just = c("center", "center"),
              width = square_size,
              height = square_size,
              gp = gpar(fill = rgb(red = runif(1),
                                   green = runif(1),
                                   blue = runif(1)),
                        col = NA),
              default.units = "in")
    grid.rect(x = (2.5 + (x - 16) * square_size),
              y = 7.5 - (max_turns - turns) * square_size,
              just = c("center", "center"),
              width = 7/300,
              height = 7/300,
              gp = gpar(fill = rgb(red = runif(1),
                                   green = runif(1),
                                   blue = runif(1)),
                        col = NA),
              default.units = "in")
    grid.rect(x = (2.5 + (x - 16) * square_size),
              y = 7.5 - (max_turns - turns) * square_size,
              just = c("center", "center"),
              width = 5/300 - .01,
              height = 5/300 - .01,
              gp = gpar(fill = rgb(red = runif(1),
                                   green = runif(1),
                                   blue = runif(1)),
                        col = NA),
              default.units = "in")
  }
  
}
grid.text(label = titles[1],
          x = 5 - .5,
          y = .5,
          just = c("right", "bottom"),
          gp = gpar(fontsize = 6),
          default.units = "in")

invisible(dev.off())

7/300 

square_size <- 13
agg_png(here("test.png"), width = 5 * 300, height = 8 * 300, units = 'px', res = 300)
for(x in 1:32) {
  for(turns in 1:146) {
    grid.rect(x = ((300 * 5 / 2) + (x - 16) * square_size),
              y = (7.5 * 300) - (max_turns - turns) * square_size,
              just = c("center", "top"),
              width = square_size,
              height = square_size,
              gp = gpar(color = "black"),
              default.units = "px")
    print(paste("x is ", ((300 * 5 / 2) + (x - 16) * square_size),
                "y is ", (7.5 * 300) - (max_turns - turns) * square_size))
  }
}
grid.text(label = titles[1],
          x = (5 - .5) * 300,
          y = .5 * 300,
          just = c("right", "bottom"),
          gp = gpar(fontsize = 6),
          default.units = "npc")

invisible(dev.off())

1/72.27

13 / (5 * 300)

?unit

text_this <- textGrob(label = titles[1],
                      x = 5 / 2,
                      y = .5,
                      just = c("left", "bottom"),
                      gp = gpar(fontsize = .04 * 300),
                      default.units = "in")


# 
# for(i in seq_along(game_names)) {
#   
#   ggplot() +
#     geom_rect(data = games[[i]],
#               aes(xmin = x - .5,
#                   xmax = x + .5,
#                   ymin = turn - .5,
#                   ymax = turn + .5,
#                   fill = color_value)) +
#     geom_rect(data = games[[i]][!is.na(games[[i]]$captured_x), ],
#               aes(xmin = x,
#                   xmax = captured_x,
#                   ymin = turn - .1,
#                   ymax = turn + .1,
#                   fill = color_value)) +
#     geom_rect(data = games[[i]][!is.na(games[[i]]$captured_x), ],
#               aes(xmin = captured_x - .33,
#                   xmax = captured_x + .33,
#                   ymin = turn - .33,
#                   ymax = turn + .33,
#                   fill = color_value)) +
#     geom_rect(data = gaps,
#               aes(xmin = x - .1,
#                   xmax = x + .1,
#                   ymin = -.5,
#                   ymax = .25),
#               fill = "white") +
#     scale_y_reverse(limits = c(max_turns + 3, -.5)) +
#     scale_fill_identity() +
#     scale_color_identity() +
#     coord_equal() +
#     theme_void() +
#     annotate("text", 
#              x = 32 + .5, 
#              y = max_turns + 2,
#              label = titles[[i]],
#              hjust = 1,
#              size = 1.5)
#   
#   ggsave(gsub("pgn", "png", game_names[[i]]),
#          device = "png",
#          width = 5,
#          height = 8,
#          units = "in",
#          dpi = 300)
# }

#
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
#
