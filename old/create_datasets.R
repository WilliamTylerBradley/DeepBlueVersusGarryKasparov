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

gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                         24.5, 26.5, 28.5, 30.5, 31.5))

for(i in seq_along(game_names)) {
  
  ggplot() +
    geom_rect(data = games[[i]],
              aes(xmin = x - .5,
                  xmax = x + .5,
                  ymin = turn - .5,
                  ymax = turn + .5,
                  fill = hex_color)) +
    geom_rect(data = games[[i]][!is.na(games[[i]]$captured_x), ],
              aes(xmin = x,
                  xmax = captured_x,
                  ymin = turn - .1,
                  ymax = turn + .1,
                  fill = hex_color)) +
    geom_rect(data = games[[i]][!is.na(games[[i]]$captured_x), ],
              aes(xmin = captured_x - .33,
                  xmax = captured_x + .33,
                  ymin = turn - .33,
                  ymax = turn + .33,
                  fill = hex_color)) +
    geom_rect(data = gaps,
              aes(xmin = x - .1,
                  xmax = x + .1,
                  ymin = -.5,
                  ymax = .25),
              fill = "white") +
    scale_y_reverse(limits = c(max_turns + 3, -.5)) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_equal() +
    theme_void() +
    annotate("text", 
             x = 32 + .5, 
             y = max_turns + 2,
             label = titles[[i]],
             hjust = 1,
             size = 1.5)
  
  ggsave(gsub("pgn", "png", game_names[[i]]),
         device = "png",
         width = 5,
         height = 8,
         units = "in",
         dpi = 300)
}


