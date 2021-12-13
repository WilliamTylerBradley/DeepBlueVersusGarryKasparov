#install.packages("rchess")
library(rchess)
library(here)
library(data.table)
library(zoo)
library(ggplot2)
library(colorspace)

pgn <- paste(readLines(here("deep_blue_kasparov_1996_5.pgn"), warn = FALSE), collapse = "\n")
chsspgn <- Chess$new()
chsspgn$load_pgn(pgn)

details <- as.data.table(chsspgn$history_detail())

starting <- details[piece_number_move == 1, .(piece, from)
                    ][, turn := 0]
setnames(starting, old = "from", new = "coords")

game <- CJ(piece = unique(details$piece), 
           turn = seq(1, max(details$number_move, na.rm = TRUE)))
game[, coords := NA]

game <- rbind(starting, game)

result <- strsplit(chsspgn$pgn(), "\n")[[1]][6]
if(result == "[Result \"1-0\"]") {
  game <- rbind(game, data.table(piece = "White King",
                                 coords = NA,
                                 turn = max(game$turn) + 1))
} else if(result == "[Result \"0-1\"]") {
  game <- rbind(game, data.table(piece = "Black King",
                                 coords = NA,
                                 turn = max(game$turn) + 1))
}

moves <- details[!is.na(number_move), .(piece, to, number_move)]
setnames(moves, old = c("number_move"), new = c("turn"))

game <- merge(game, moves, all.x = TRUE, all.y = TRUE)
game[, ':=' (coords = ifelse(is.na(coords), to, coords),
             to = NULL)]
setkey(game, piece, turn)

game[, coords := na.locf(coords), by = piece]

captures <- details[status == 'captured', .(piece, number_move_capture, captured_by)] 
setnames(captures, old = c("piece", "number_move_capture"),
         new = c("captured_piece", "turn"))
# add this info back in
game <- merge(game, captures, by.x = c("piece", "turn"), 
               by.y = c("captured_piece", "turn"), all.x = TRUE, all.y = TRUE)
game <- merge(game, captures, by.x = c("piece", "turn"), 
              by.y = c("captured_by", "turn"), all.x = TRUE, all.y = TRUE)
game[, captured_by := na.locf(captured_by, na.rm = FALSE), by = piece]
game <- game[is.na(captured_by), ][, captured_by := NULL]

spacing <- 0
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
                          x = cumsum(rep(1, 32) + 
                                       c(0, spacing, spacing, 0, spacing, 0, spacing, 0, spacing, rep(0, 7),
                                         spacing, rep(0, 7), spacing, 0, spacing, 0, spacing, 0, spacing, spacing)))

game <- merge(game, piece_order, all = TRUE, by = c("piece"))
game <- merge(game, piece_order, all.x = TRUE, by.x = c("captured_piece"), by.y = c("piece"))
setnames(game, old = c("x.x", "x.y"), new = c("x", "captured_x"))

h_values <- seq(0, 100, length.out = 8) + 75
l_values <- seq(75, 25, length.out = 8)
c_values <- seq(min(max_chroma(h = h_values, l = l_values[8])), 
                min(max_chroma(h = h_values, l = l_values[1])), length.out = 8)

color_set <- data.table(h_values = rep(h_values, each = 8),
                        l_values = rep(l_values, 8),
                        c_values = rep(c_values, 8),
                        file = rep(letters[1:8], each = 8),
                        rank = rep(seq(1, 8), 8))
color_set[, color_value := hcl(h_values, c_values, l_values)]

ggplot(data = color_set,
       aes(x = file,
           y = rank,
           fill = color_value)) +
  geom_tile() +
  scale_fill_identity()

color_set[, coords := paste0(file, rank)]
game <- merge(game, color_set, by.x = c("coords"),
              by.y = c("coords"))

gaps <- data.table(x = c(1.5, 2.5, 4.5, 6.5, 8.5, 16.5,
                         24.5, 26.5, 28.5, 30.5, 31.5),
                   y = -1,
                   color_value = "#FFFFFF")

# ggplot(data = game,
#        aes(x = x,
#            y = turn,
#            fill = color_value)) +
#   geom_tile() +
#   geom_segment(aes(x = x,
#                    y = turn,
#                    xend = captured_x,
#                    yend = turn,
#                    color = color_value)) +
#   geom_segment(data = gaps,
#                aes(x = x,
#                    xend = x,
#                    y = y,
#                    yend = y + 1),
#                color = "white",
#                size = 2) +
#   scale_y_reverse() +
#   scale_fill_identity() +
#   scale_color_identity() +
#   coord_equal() +
#   theme_void()


ggplot() +
  geom_rect(data = game,
            aes(xmin = x - .5,
                xmax = x + .5,
                ymin = turn - .5,
                ymax = turn + .5,
                fill = color_value)) +
  geom_rect(data = game[!is.na(game$captured_x), ],
               aes(xmin = x,
                   xmax = captured_x,
                   ymin = turn - .1,
                   ymax = turn + .1,
                   fill = color_value)) +
  geom_rect(data = game[!is.na(game$captured_x), ],
            aes(xmin = captured_x - .33,
                xmax = captured_x + .33,
                ymin = turn - .33,
                ymax = turn + .33,
                fill = color_value)) +
  geom_segment(data = gaps,
               aes(x = x,
                   xend = x,
                   y = y,
                   yend = y + 1),
               color = "white",
               size = 2) +
  scale_y_reverse() +
  scale_fill_identity() +
  scale_color_identity() +
  coord_equal() +
  theme_void()

## need to get 72 dpi to match nicely for 32 rect
## 72 / 32 = 2.25



# ggsave("deep_blue_kasparov_1996_5.png",
#        device = "png",
#        width = 9,
#        height = 18,
#        units = "in",
#        dpi = 72)
