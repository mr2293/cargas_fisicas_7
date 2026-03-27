# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
library(readxl)
library(zoo)
library(reshape2)
library(gt)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# Leer datos ----

# datos <- read_xlsx("data/Sessions_micro01.xlsx")

datos <- read_excel(file.path("data", "Sessions_micro01.xlsx"))
datos$date <- as.Date(datos$date)

datos <-  datos |>
  mutate(player = case_when(
    player == "kevin alvarez" ~ "Kevin Álvarez",
    player == "Erick Sanchez" ~ "Erick Sánchez",
    player == "Brian Rodriguez" ~ "Brian Rodríguez",
    player == "Victor Davila" ~ "Víctor Dávila",
    player == "Miguel Ramirez" ~ "Miguel Ramírez",
    player == "Miguel  Vazquez" ~ "Miguel Vázquez",
    player == "Nestor Araujo" ~ "Néstor Araujo",
    # player == "Fidalgo Fidalgo" ~ "Álvaro Fidalgo",
    player == "Jona Dos Santos" ~ "Jonathan Dos Santos",
    player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
    player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
    player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
    player == "Isaias Violante" ~ "Isaías Violante",
    player == "Jose Zuniga" ~ "José Raúl Zúñiga",
    # player == "Allan Maximin" ~ "Allan Saint-Maximin",
    player == "Patricio Salas" ~ "Patricio Salas",
    player == "Rodrigo Dourado" ~ "Rodrigo Dourado",
    player == "Aaron Mejia" ~ "Aarón Mejía",
    player == "Thiago Espinosa" ~ "Thiago Espinosa",
    player == "Vinicius Lima" ~ "Vinícius Lima",
    TRUE ~ player
  ),
  date = as.Date(date))

# Velocidades Máximas Jugadores ----

vel_max_lookup <- tibble::tribble(
  ~player,                ~vel_max_hist,
  "Aarón Mejía",               35.51,
  "Alan Cervantes",            33.55,
  "Alejandro Zendejas",        34.00,
  "Alexis Gutiérrez",          33.57,
  # "Allan Saint-Maximin",       35.39,
  "Brian Rodríguez",           35.60,
  "Cristian Borja",            34.00,
  "Dagoberto Espinoza",        35.58,
  "Erick Sánchez",             33.05,
  "Henry Martín",              33.71,
  "Igor Lichnovsky",           34.00,
  "Isaías Violante",           35.00,
  "Israel Reyes",              33.00,
  "Jonathan Dos Santos",       31.47,
  "José Raúl Zúñiga",          34.93,
  "Kevin Álvarez",             35.00,
  "Miguel Vázquez",            34.25,
  "Néstor Araujo",             33.00,
  "Patricio Salas",            35.00,
  "Ramón Juárez",              33.00,
  # "Rodrigo Aguirre",           34.45,
  "Rodrigo Dourado",           30.00,
  "Santiago Naveda",           32.45,
  "Sebastián Cáceres",         35.00,
  "Víctor Dávila",             34.00,
  "Raphael Veiga",             32.00,
  "Vinícius Lima",             31.66,
  "Thiago Espinosa",           34.00
  # "Álvaro Fidalgo",            33.50
)

selected_players <- c("Aarón Mejía", "Alan Cervantes", "Alejandro Zendejas", "Alexis Gutiérrez", "Brian Rodríguez",           
                   "Cristian Borja", "Dagoberto Espinoza", "Erick Sánchez", "Henry Martín", "Isaías Violante",           
                   "Israel Reyes", "Jonathan Dos Santos", "José Raúl Zúñiga", "Kevin Álvarez", "Miguel Vázquez",      
                   "Néstor Araujo", "Patricio Salas", "Ramón Juárez", "Rodrigo Dourado", "Santiago Naveda",           
                   "Sebastián Cáceres", "Víctor Dávila", "Raphael Veiga", "Vinícius Lima", "Thiago Espinosa")

datos <- datos |>
  left_join(vel_max_lookup, by = "player") |>
  filter(player %in% selected_players) |>
  filter(player != "Luis Ángel Malagón")

# -------------------------------------------------------
# Últimos 7 días calendario (no sesiones) ----
# -------------------------------------------------------

end_date_7d   <- max(datos$date, na.rm = TRUE)
start_date_7d <- end_date_7d - days(6)   # 7 días inclusive

last7 <- datos |>
  filter(date >= start_date_7d, date <= end_date_7d) |>
  group_by(player) |>
  summarise(
    HSR_abs_dist_7d     = sum(HSR_abs_dist,     na.rm = TRUE),
    distance_abs_7d     = sum(distance_abs,     na.rm = TRUE),
    sprints_abs_count_7d = sum(sprints_abs_count, na.rm = TRUE),
    .groups = "drop"
  )

datos |>
  filter(date >= start_date_7d, date <= end_date_7d) |>
  distinct(date) |>
  arrange(date)

datos |>
  filter(date >= start_date_7d, date <= end_date_7d) |>
  count(date) |>
  arrange(date)

# # Acumulado HSR y Sprint por Jugador ----
# 
# # 1) Define the last 7 days window based on the latest date in the dataset
# end_date <- max(datos$date, na.rm = TRUE)
# start_date <- end_date - days(6)  # last 7 days inclusive
# 
# # 2) Sum distances over last 7 days per player
# df_7d <- datos |>
#   filter(date >= start_date, date <= end_date) |>
#   group_by(player) |>
#   summarise(
#     HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE),
#     distance_abs = sum(distance_abs, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   mutate(total_dist = HSR_abs_dist + distance_abs)
# 
# # 3) Pivot long for stacked bars + set player order by total descending
# df_long <- df_7d |>
#   pivot_longer(
#     cols = c(HSR_abs_dist, distance_abs),
#     names_to = "metric",
#     values_to = "value"
#   ) |>
#   mutate(
#     metric = recode(
#       metric,
#       HSR_abs_dist = "Distancia en HSR",
#       distance_abs = "Distancia en Sprint"
#     ),
#     player = reorder(player, df_7d$total_dist[match(player, df_7d$player)])
#   )
# 
# # 4) Plot
# ggplot(df_long, aes(x = player, y = value, fill = metric)) +
#   geom_col() +
#   geom_text(
#     aes(label = round(value, 0)),
#     position = position_stack(vjust = 0.5),
#     color = "white",
#     size = 3,
#     fontface = "bold"
#   ) +
#   coord_flip() +
#   scale_fill_manual(
#     values = c(
#       "Distancia en Sprint" = "#0B1B4A",  # navy blue
#       "Distancia en HSR" = "#C1121F" # red
#     )
#   ) +
#   scale_y_continuous(
#     labels = scales::comma,
#     expand = expansion(mult = c(0, 0.05))
#   ) + 
#   labs(
#     title = "Distancia en HSR + Sprint (Últimos 7 Días)",
#     x = NULL,
#     y = "Distancia (m)",
#     fill = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     # --- Title ---
#     plot.title = element_text(size = 18, face = "bold", margin = margin(b = 12)),
#     # --- Axis text ---
#     axis.text.y = element_text(size = 13, face = "bold"),
#     axis.text.x = element_text(size = 12, color = "grey30"),
#     # --- Axis titles ---
#     axis.title.x = element_text(size = 13, margin = margin(t = 10)),
#     axis.line.x = element_blank(),
#     # --- Grid cleanup ---
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
#     # --- Legend ---
#     legend.position = "top",
#     legend.text = element_text(size = 13, face = "bold"),
#     legend.title = element_blank(),
#     legend.key.height = unit(0.5, "cm"),
#     # --- Background ---
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background  = element_rect(fill = "white", color = NA),
#     # --- Margins ---
#     plot.margin = margin(14, 18, 14, 18)
#   )
# 
# # Acumulado Distancia Total por Jugador ---
# 
# # --- last 7 days window (inclusive) ---
# end_date <- max(datos$date, na.rm = TRUE)
# start_date <- end_date - days(6)
# 
# # --- sum distance_m per player over last 7 days ---
# df_dist7 <- datos |>
#   filter(date >= start_date, date <= end_date) |>
#   group_by(player) |>
#   summarise(distance_m_7d = sum(distance_m, na.rm = TRUE), .groups = "drop") |>
#   arrange(desc(distance_m_7d)) |>
#   mutate(player = factor(player, levels = rev(player)))  # keeps descending top-to-bottom
# 
# # --- team average (now df_dist7 exists) ---
# team_avg <- mean(df_dist7$distance_m_7d, na.rm = TRUE)
# 
# # --- safe x location for label (won't clip) ---
# x_label <- levels(df_dist7$player)[length(levels(df_dist7$player))]
# 
# ggplot(df_dist7, aes(x = player, y = distance_m_7d)) +
#   geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
#   geom_hline(
#     yintercept = team_avg,
#     color = "#C1121F",
#     linetype = "dashed",
#     linewidth = 1.2
#   ) +
#   geom_text(
#     aes(label = paste0(round(distance_m_7d / 1000), "k")),
#     color = "white",
#     size = 4,
#     fontface = "bold",
#     hjust = 1.05
#   ) +
#   annotate(
#     "text",
#     y = team_avg,
#     x = x_label,  # <- use a real x so it always renders
#     label = paste0("Promedio equipo: ", round(team_avg / 1000, 1), "k"),
#     color = "#C1121F",
#     fontface = "bold",
#     size = 5,
#     hjust = -0.4,
#     vjust = 45
#   ) +
#   coord_flip() +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Distancia Total (Últimos 7 Días)",
#     x = NULL,
#     y = "Distancia (m)"
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title = element_text(face = "bold", size = 22, margin = margin(b = 12)),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14, face = "bold"),
#     axis.line = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
#     plot.margin = margin(15, 30, 15, 20)   # a bit more right margin for comfort
#   )
# 
# # Acumulado ACC + DECC por Jugador ----
# 
# # --- last 7 days window (inclusive) ---
# end_date <- max(datos$date, na.rm = TRUE)
# start_date <- end_date - days(6)
# 
# # --- sum acc_plus_decc over last 7 days per player ---
# df_acc7 <- datos |>
#   filter(date >= start_date, date <= end_date) |>
#   group_by(player) |>
#   summarise(acc_plus_decc_7d = sum(acc_plus_decc, na.rm = TRUE), .groups = "drop") |>
#   arrange(desc(acc_plus_decc_7d)) |>
#   mutate(player = factor(player, levels = rev(player)))  # keeps descending top-to-bottom
# 
# # --- team average ---
# team_avg_acc <- mean(df_acc7$acc_plus_decc_7d, na.rm = TRUE)
# 
# # --- safe x location for label (won't clip) ---
# x_label_acc <- levels(df_acc7$player)[length(levels(df_acc7$player))]
# 
# # --- bar chart: Acc + Decc (Últimos 7 Días) ---
# ggplot(df_acc7, aes(x = player, y = acc_plus_decc_7d)) +
#   geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
#   geom_hline(
#     yintercept = team_avg_acc,
#     color = "#C1121F",
#     linetype = "dashed",
#     linewidth = 1.2
#   ) +
#   geom_text(
#     aes(label = scales::comma(round(acc_plus_decc_7d, 0))),
#     color = "white",
#     size = 4,
#     fontface = "bold",
#     hjust = 1.05
#   ) +
#   annotate(
#     "text",
#     y = team_avg_acc,
#     x = x_label_acc,
#     label = paste0("Promedio equipo: ", scales::comma(round(team_avg_acc, 0))),
#     color = "#C1121F",
#     fontface = "bold",
#     size = 5,
#     hjust = -0.4,
#     vjust = 45
#   ) +
#   coord_flip() +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "ACC + DECC (Últimos 7 Días)",
#     x = NULL,
#     y = "ACC + DECC (Acumulado)"
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title = element_text(face = "bold", size = 22, margin = margin(b = 12)),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14, face = "bold"),
#     axis.line = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
#     plot.margin = margin(15, 30, 15, 20)
#   )
# 
# # Acumulado Player Load por Jugador ----
# 
# # --- last 7 days window (inclusive) ---
# end_date <- max(datos$date, na.rm = TRUE)
# start_date <- end_date - days(6)
# 
# # --- sum player_load over last 7 days per player ---
# df_pl7 <- datos |>
#   filter(date >= start_date, date <= end_date) |>
#   group_by(player) |>
#   summarise(player_load_7d = sum(player_load, na.rm = TRUE), .groups = "drop") |>
#   arrange(desc(player_load_7d)) |>
#   mutate(player = factor(player, levels = rev(player)))  # keeps descending top-to-bottom
# 
# # --- team average ---
# team_avg_pl <- mean(df_pl7$player_load_7d, na.rm = TRUE)
# 
# # --- safe x location for label (won't clip) ---
# x_label_pl <- levels(df_pl7$player)[length(levels(df_pl7$player))]
# 
# # --- bar chart: Player Load (Últimos 7 Días) ---
# ggplot(df_pl7, aes(x = player, y = player_load_7d)) +
#   geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
#   geom_hline(
#     yintercept = team_avg_pl,
#     color = "#C1121F",
#     linetype = "dashed",
#     linewidth = 1.2
#   ) +
#   geom_text(
#     aes(label = scales::comma(round(player_load_7d, 0))),
#     color = "white",
#     size = 4,
#     fontface = "bold",
#     hjust = 1.05
#   ) +
#   annotate(
#     "text",
#     y = team_avg_pl,
#     x = x_label_pl,
#     label = paste0("Promedio equipo: ", scales::comma(round(team_avg_pl, 0))),
#     color = "#C1121F",
#     fontface = "bold",
#     size = 5,
#     hjust = -0.4,
#     vjust = 45
#   ) +
#   coord_flip() +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Player Load (Últimos 7 Días)",
#     x = NULL,
#     y = "Player Load"
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title = element_text(face = "bold", size = 22, margin = margin(b = 12)),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14, face = "bold"),
#     axis.line = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
#     plot.margin = margin(15, 30, 15, 20)
#   )
# 
# # % Velocidad Máxima Promedio por Jugador ----
# 
# # --- last 7 days window (inclusive) ---
# end_date   <- max(datos$date, na.rm = TRUE)
# start_date <- end_date - days(6)
# 
# # --- per player: max speed in last 7 days + historical top speed ---
# df_speed7 <- datos |>
#   filter(date >= start_date, date <= end_date) |>
#   group_by(player) |>
#   summarise(
#     max_speed_7d  = max(max_speed, na.rm = TRUE),
#     vel_max_hist  = max(vel_max_hist, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   mutate(
#     pct_hist_7d = 100 * (max_speed_7d / vel_max_hist)
#   ) |>
#   # guard against weird values / missing hist speeds
#   mutate(
#     pct_hist_7d = if_else(is.finite(pct_hist_7d) & vel_max_hist > 0, pct_hist_7d, NA_real_)
#   ) |>
#   arrange(desc(pct_hist_7d)) |>
#   mutate(player = factor(player, levels = rev(player)))
# 
# # --- average % across players (last 7 days) ---
# team_avg_pct <- mean(df_speed7$pct_hist_7d, na.rm = TRUE)
# 
# # --- label anchor (avoid clipping) ---
# x_label <- levels(df_speed7$player)[length(levels(df_speed7$player))]
# 
# # --- bar chart: % of historical top speed reached in last 7 days ---
# ggplot(df_speed7, aes(x = player, y = pct_hist_7d)) +
#   geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
#   geom_hline(
#     yintercept = team_avg_pct,
#     color = "#C1121F",
#     linetype = "dashed",
#     linewidth = 1.2
#   ) +
#   geom_text(
#     aes(label = paste0(round(pct_hist_7d, 0), "%")),
#     color = "white",
#     size = 4,
#     fontface = "bold",
#     hjust = 1.05
#   ) +
#   annotate(
#     "text",
#     y = team_avg_pct,
#     x = x_label,
#     label = paste0("Promedio equipo: ", round(team_avg_pct, 1), "%"),
#     color = "#C1121F",
#     fontface = "bold",
#     size = 3.5,
#     hjust = 1.1,
#     vjust = 69
#   ) +
#   coord_flip() +
#   scale_y_continuous(
#     labels = ~ paste0(scales::comma(.x), "%"),
#     limits = c(0, NA)
#   ) +
#   labs(
#     title = "% de Velocidad Máxima Histórica Alcanzada (Últimos 7 Días)",
#     x = NULL,
#     y = "Porcentaje de Velocidad Máxima Histórica"
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title = element_text(face = "bold", size = 22, margin = margin(b = 12)),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14, face = "bold"),
#     axis.line = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
#     plot.margin = margin(15, 30, 15, 20)
#   )