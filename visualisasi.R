library(tidyverse)
library(readxl)
library(scales)


colors <- c("#AA5F81", "#F68838", "#E8B85A")


df_pertumbuhan_q <- read_excel(
  "data/olah-data.xlsx",
  sheet = "pertumbuhan_q_to_q"
)

# df_pertumbuhan_q <- df_pertumbuhan_q %>%
#   filter(!is.na(Kategori)) %>%
#   select(-Kategori) %>%
#   pivot_longer(-Uraian, names_to = "triwulan", values_to = "nilai") %>%
#   mutate(
#     x = rep(1:8, 3),
#     Uraian = as.factor(Uraian), #, levels = c("Primer", "Sekunder", "Tersier"),
#     vjust = if_else(nilai < 0, 1.05, -1.05),
#     label = scales::number(round(nilai, 2), decimal.mark = ",")
#   )
# levels(df_pertumbuhan_q$Uraian) <- c("Primer", "Sekunder", "Tersier")

triwulan <- c(
  "Q1 2024",
  "Q2 2024",
  "Q3 2024",
  "Q4 2024",
  "Q1 2025",
  "Q2 2025",
  "Q3 2025",
  "Q4 2025"
)

cleaning_data_line <- function(df = NULL) {
  df_clean <- df %>%
    filter(Uraian != "PRODUK DOMESTIK REGIONAL BRUTO") %>%
    # select(-Kategori) %>%
    pivot_longer(-Uraian, names_to = "triwulan", values_to = "nilai") %>%
    mutate(
      x = rep(1:8, 3),
      Uraian = as.factor(Uraian), #, levels = c("Primer", "Sekunder", "Tersier")
      vjust = if_else(nilai < 0, 1.15, -0.75),
      label = scales::number(nilai, decimal.mark = ",")
    )
  levels(df$Uraian) <- c("Primer", "Sekunder", "Tersier")

  df_clean
}

df_pertumbuhan_q <- cleaning_data_line(df_pertumbuhan_q)
# ggplot(
#   df_pertumbuhan_q,
#   aes(x = x, y = nilai, color = Uraian, , group = Uraian)
# ) +
#   geom_line(show.legend = FALSE, linewidth = 1) +
#   geom_point(size = 2) +
#   geom_text(
#     aes(label = round(nilai, 2)),
#     color = "#575757",
#     size = 3,
#     hjust = 0.5,
#     vjust = -0.75
#   ) +
#   labs(x = "", y = "", color = "") +
#   scale_color_manual(values = colors) +
#   scale_x_continuous(breaks = seq(1, 8, 1), labels = triwulan) +
#   theme_minimal() +
#   theme(
#     # plot.background = element_rect(fill = "white"),
#     # panel.background = element_rect(fill = "white"),
#     panel.grid.major.y = element_line(color = "#d3d2d2", linewidth = 0.25),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     legend.position = "bottom",
#     axis.line.x = element_line(color = "#575757", linewidth = 1),
#     axis.ticks.x = element_line(color = "#575757", linewidth = 1)
#   )

line_chart <- function(df = NULL) {
  ggplot(
    df,
    aes(x = x, y = nilai, color = Uraian, , group = Uraian)
  ) +
    geom_line(show.legend = FALSE, linewidth = 1.25) +
    geom_point(size = 2) +
    geom_text(
      aes(label = label, vjust = vjust),
      color = "#575757",
      size = 3,
      hjust = 0.5,
      # vjust = -0.75
    ) +
    labs(x = "", y = "", color = "") +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(1, 8, 1), labels = triwulan) +
    theme_minimal() +
    theme(
      # plot.background = element_rect(fill = "white"),
      # panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "#d3d2d2", linewidth = 0.25),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      axis.line.x = element_line(color = "#575757", linewidth = 1),
      axis.ticks.x = element_line(color = "#575757", linewidth = 1, length = 3),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      margins = margin(t = 15, r = 5, b = 10, l = 5, unit = "pt")
    )
}

line_chart(df_pertumbuhan_q)
