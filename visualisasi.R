library(tidyverse)
library(readxl)
library(scales)
library(janitor)


colors <- c("#AA5F81", "#F68838", "#E8B85A")

colors4 <- c("#795B85", "#AA5F81", "#F68838", "#E8B85A")

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
      label = scales::number(round(nilai, 2), decimal.mark = ",")
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
      check_overlap = TRUE
      # vjust = -0.75
    ) +
    labs(x = "", y = "Pertumbuhan (%)", color = "") +
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
      legend.position = "top",
      axis.line.x = element_line(color = "#575757", linewidth = 1),
      axis.ticks.x = element_line(color = "#575757", linewidth = 1),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      margins = margin(t = 15, r = 5, b = 10, l = 5, unit = "pt")
    )
}

line_chart(df_pertumbuhan_q)


df_distribusi <- read_excel("data/olah-data.xlsx", sheet = "distribusi")


# df_distribusi <- df_distribusi %>%
#   mutate(kumulative = cumsum(nilai))

cleaning_data_pie <- function(df = NULL) {
  df <- df %>%
    filter(Uraian != "PRODUK DOMESTIK REGIONAL BRUTO") %>%
    mutate(Uraian = as.factor(Uraian)) %>%
    pivot_longer(-Uraian, names_to = "triwulan", values_to = "nilai") %>%
    filter(triwulan == "Q4 2025") %>%
    mutate(
      ymax = cumsum(nilai),
      label_nilai = number(nilai, decimal.mark = ","),
      label = paste0(Uraian, "\n", label_nilai, "%")
    )

  df$ymin = c(0, head(df$ymax, n = -1))
  df$label_position = (df$ymin + df$ymax) / 2

  levels(df$Uraian) <- c("Primer", "Sekunder", "Tersier")

  df
}
# Pie chart:

# ggplot(df_distribusi, aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = Uraian)) +
#   geom_rect(show.legend = FALSE) +
#   geom_text(aes(y = label_position, label = label), x = 3.5, size = 3.5, color = "white") +
#   coord_polar(theta = "y") +
#   scale_fill_manual(values = colors) +
#   xlim(c(2, 4)) +
#   theme_void()

pie_chart <- function(df = NULL, label_color = "white", label_size = 3.5) {
  ggplot(df, aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = Uraian)) +
    geom_rect(show.legend = FALSE) +
    geom_text(
      aes(y = label_position, label = label),
      x = 3.5,
      size = label_size,
      color = label_color
    ) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors) +
    xlim(c(2, 4)) +
    theme_void()
}

# pie_chart(df_distribusi, label_size = 4)

df_pertumbuhan_pengeluaran_q <- read_excel(
  "data/8101 PDRB Pengeluaran TW 4 2025.xlsx",
  sheet = "pertumbuhan_q_to_q"
)

clean_data_pengeluaran <- function(df = NULL) {
  df <- df %>%
    pivot_longer(
      -`Komponen Pengeluaran`,
      names_to = "triwulan",
      values_to = "nilai"
    ) %>%
    mutate(
      kategori = case_when(
        `Komponen Pengeluaran` ==
          "1. Pengeluaran Konsumsi Rumah Tangga" ~ "PKRT",
        `Komponen Pengeluaran` == "3. Pengeluaran Konsumsi Pemerintah" ~ "PKP",
        `Komponen Pengeluaran` == "4. Pembentukan Modal Tetap Bruto" ~ "PMTB",
        TRUE ~ "Lainnya"
      )
    ) %>%
    clean_names() %>%
    mutate(
      triwulan = str_replace(triwulan, pattern = "-", replacement = " ")
    ) %>%
    mutate(
      triwulan = factor(triwulan),
      kategori = factor(kategori),
      label = number(round(nilai, 2), decimal.mark = ","),
      vjust = if_else(nilai < 0, 1.05, -0.5),
      color = case_when(
        kategori == "PKRT" ~ "#AA5F81",
        kategori == "PKP" ~ "#F68838",
        kategori == "PMTB" ~ "#E8B85A",
        TRUE ~ "#ABABAB"
      )
    ) %>%
    filter(kategori != "Lainnya")

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

  levels(df$triwulan) <- triwulan
  df
}
# kategori <- c("PKP", "PKRT", "PMTB", "Lainnya")
# levels(df_pertumbuhan_pengeluaran_q$kategori) <- kategori
df_pertumbuhan_pengeluaran_q <- clean_data_pengeluaran(
  df_pertumbuhan_pengeluaran_q
)

colors2 <- df_pertumbuhan_pengeluaran_q %>%
  select(kategori, color) %>%
  distinct(kategori, color)


colors3 <- colors2$color
names(colors3) <- colors2$kategori

line_chart_pengeluaran <- function(
  df = NULL,
  label_color = "#9A9A9A",
  label_size = 2.5
) {
  ggplot(
    df,
    aes(
      x = triwulan,
      y = nilai,
      color = kategori,
      group = komponen_pengeluaran
    )
  ) +
    geom_line(linewidth = 1.25, show.legend = FALSE) +
    geom_point(size = 2) +
    geom_text(
      aes(label = label, vjust = vjust),
      color = label_color,
      size = label_size,
      hjust = 0,
      check_overlap = TRUE
    ) +
    scale_color_manual(values = colors3) +
    labs(
      x = "",
      y = "Pertumbuhan (%)",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "#575757", linewidth = 1),
      axis.ticks.x = element_line(color = "#575757", linewidth = 1),
      margins = margin(t = 15, r = 5, b = 10, l = 5, unit = "pt"),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
    )
}

# Distribusi pengeluaran triwulan IV 2025

df_distribusi_2025 <- read_excel(
  "data/8101 PDRB Pengeluaran TW 4 2025.xlsx",
  sheet = "distribusi"
)

clean_distribusi <- function(df = NULL) {
  df <- df %>%
    clean_names() %>%
    mutate(
      kategori = case_when(
        komponen_pengeluaran == "1. Pengeluaran Konsumsi Rumah Tangga" ~ "PKRT",
        komponen_pengeluaran == "3. Pengeluaran Konsumsi Pemerintah" ~ "PKP",
        komponen_pengeluaran == "4. Pembentukan Modal Tetap Bruto" ~ "PMTB",
        TRUE ~ "Lainnya"
      ),
    ) %>%
    group_by(kategori) %>%
    summarise(
      nilai = round(sum(q4_2025), 2)
    ) %>%
    mutate(
      label = paste0(number(nilai, decimal.mark = ","), "%"),
      hjust = if_else(nilai < 0, 1.25, -0.25)
    )
}

bar_chart <- function(df = NULL) {
  ggplot(
    df_distribusi_2025,
    aes(y = fct_reorder(kategori, nilai), x = nilai, fill = kategori)
  ) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = label, hjust = hjust),
      x = 0,
      color = "white",
      size = 4.5,
      vjust = 0.5,
    ) +
    scale_fill_manual(values = colors4) +
    labs(
      x = "",
      y = "",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none",
      legend.key.size = unit(10, "pt"),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      margins = margin(t = 15, r = 5, b = 10, l = 5, unit = "pt")
    )
}
