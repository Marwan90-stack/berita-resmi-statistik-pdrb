library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(gt)
# library(ggtext)

colors <- c("#AA5F81", "#F68838", "#E8B85A")

colors4 <- c("#795B85", "#AA5F81", "#F68838", "#E8B85A")

df_pertumbuhan_q <- read_excel(
  "data/olah-data.xlsx",
  sheet = "pertumbuhan_q_to_q"
)

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
    # filter(Uraian != "PRODUK DOMESTIK REGIONAL BRUTO") %>%
    # select(-Kategori) %>%
    pivot_longer(-Uraian, names_to = "triwulan", values_to = "nilai") %>%
    mutate(
      x = rep(1:8, 4),
      # triwulan = as.factor(triwulan),
      Uraian = as.factor(Uraian),
      vjust = if_else(nilai < 4.2, 1.15, -0.50),
      label = scales::number(round(nilai, 2), decimal.mark = ",")
    )
  levels(df$Uraian) <- c("Primer", "Sekunder", "Tersier")
  # levels(df$triwulan) <- c(
  #   "Q1 2024",
  #   "Q2 2024",
  #   "Q3 2024",
  #   "Q4 2024",
  #   "Q1 2025",
  #   "Q2 2025",
  #   "Q3 2025",
  #   "Q4 2025"
  # )

  df_clean
}

df_pertumbuhan_q <- cleaning_data_line(df_pertumbuhan_q)

df_pertumbuhan_y <- read_excel(
  "data/olah-data.xlsx",
  sheet = "pertumbuhan_y_on_y"
)

df_pertumbuhan_y <- cleaning_data_line(df_pertumbuhan_y)

line_chart <- function(df = NULL) {
  df %>%
    filter(Uraian != "PRODUK DOMESTIK REGIONAL BRUTO") %>%
    ggplot(
      # df,
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
      legend.justification = c("left", "top"),
      axis.line.x = element_line(color = "#575757", linewidth = 1),
      axis.ticks.x = element_line(color = "#575757", linewidth = 1),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      margins = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )
}

# line_chart(df_pertumbuhan_q)

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
      vjust = if_else(nilai < 0, 1.05, -0.25),
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
      legend.justification = c("left", "top"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
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

df_pertumbuhan_pengeluaran_y <- read_excel(
  "data/8101 PDRB Pengeluaran TW 4 2025.xlsx",
  sheet = "pertumbuhan_y_on_y"
)


cleaning_data_pertumbuhan_pengeluaran <- function(df = NULL) {
  df <- df %>%
    filter(`Komponen Pengeluaran` != "P D R B") %>%
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
    filter(kategori != "Lainnya") %>%
    mutate(
      nilai = round(nilai, 2),
      label = number(nilai, decimal.mark = ","),
      triwulan = factor(triwulan),
      vjust = if_else(nilai < 2, 1.35, -0.35)
    )

  levels(df$triwulan) <- triwulan
  df
}

line_chart_pertumbuhan_pengeluaran <- function(
  df = NULL,
  label_size = 2.5,
  label_color = "#575757"
) {
  ggplot(
    df,
    aes(x = triwulan, y = nilai, color = kategori, group = kategori)
  ) +
    geom_line(show.legend = FALSE, linewidth = 1.25) +
    geom_point(size = 2) +
    geom_text(
      aes(label = label, vjust = vjust),
      color = label_color,
      size = label_size,
      hjust = 0.5,
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
      legend.justification = c("left", "top"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(color = "#575757", linewidth = 1),
      axis.ticks.x = element_line(color = "#575757", linewidth = 1),
      margins = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      # panel.background = element_rect(fill = "#FFFFFF", alpha = 0),
      # plot.background = element_rect(fill = "#FFFFFF", alpha = 0),
    )
}

df_pdrb_adhb_adhk <- read_excel(
  "data/olah-data.xlsx",
  sheet = "pdrb_adhb_adhk"
)


clean_data_adhb_adhk <- function(df = NULL) {
  df_clean <- df %>%
    # filter(Uraian != "PRODUK DOMESTIK REGIONAL BRUTO") %>%
    pivot_longer(-Uraian, names_to = "jenis", values_to = "nilai") %>%
    mutate(
      triwulan = str_extract(jenis, pattern = "Q+\\d"),
      kategori = str_remove(jenis, pattern = "Q+\\d+-"),
      nilai = round(nilai / 1000, 2),
      Uraian = if_else(
        Uraian == "PRODUK DOMESTIK REGIONAL BRUTO",
        "PDRB",
        Uraian
      )
    ) %>%
    select(-jenis) %>%
    mutate(
      kategori = if_else(kategori == "adhb", "Harga Berlaku", "Harga Konstan"),
      triwulan = case_when(
        triwulan == "Q1" ~ "Triwulan I",
        triwulan == "Q2" ~ "Triwulan II",
        triwulan == "Q3" ~ "Triwulan III",
        TRUE ~ "Triwulan IV"
      )
    )

  df_wide <- df_clean %>%
    pivot_wider(
      id_cols = Uraian,
      names_from = c(kategori, triwulan),
      values_from = nilai
    )
  df_wide
}

df_pdrb_wide <- clean_data_adhb_adhk(df_pdrb_adhb_adhk)

create_table_adhb_adhk <- function(df_wide = NULL) {
  df_wide %>%
    gt() %>%
    gt::fmt_number(dec_mark = ",", sep_mark = ".") %>%
    gt::tab_spanner(
      label = "Harga Berlaku",
      columns = starts_with("Harga Berlaku")
    ) %>%
    gt::tab_spanner(
      label = "Harga Konstan",
      columns = starts_with("Harga Konstan")
    ) %>%
    # tab_header(
    #   title = "PDRB Atas Dasar Harga Berlaku dan harga Konstan 2010 Menurut Sektor Triwulan I - IV 2025"
    # ) %>%
    gt::cols_label(
      `Harga Berlaku_Triwulan I` = "Triwulan I 2025",
      `Harga Berlaku_Triwulan II` = "Triwulan II 2025",
      `Harga Berlaku_Triwulan III` = "Triwulan III 2025",
      `Harga Berlaku_Triwulan IV` = "Triwulan IV 2025",
      `Harga Konstan_Triwulan I` = "Triwulan I 2025",
      `Harga Konstan_Triwulan II` = "Triwulan II 2025",
      `Harga Konstan_Triwulan III` = "Triwulan III 2025",
      `Harga Konstan_Triwulan IV` = "Triwulan IV 2025",
      Uraian = "Komponen"
    ) %>%
    data_color(
      colors = c("#AA5F81", "#f1c161"),
      columns = !contains("Uraian")
    ) %>%
    data_color(
      colors = colors[2],
      columns = contains("Uraian")
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_labels()
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_spanners()
    ) %>%
    tab_options(
      table.border.top.color = "white",
      table.border.right.color = "white",
      table.border.bottom.color = "white",
      table.border.left.color = "white",

      table_body.hlines.color = "white",
      table_body.vlines.color = "white",
      heading.border.bottom.color = "white",

      table.border.top.width = px(1),
      table.border.bottom.width = px(1),
      table.font.size = "10pt",

      data_row.padding = px(20),
      column_labels.padding = px(25),
    )
}

# Gabungkan data pertumbuhan lapangan usaha y-to-y dan q-to-q
df_pertumbuhan_y_34 <- df_pertumbuhan_y %>%
  filter(triwulan == "Q3 2025" | triwulan == "Q4 2025") %>%
  select(Uraian, triwulan, nilai) %>%
  mutate(kategori = rep("Y to Y", 8))

df_pertumbuhan_q_34 <- df_pertumbuhan_q %>%
  filter(triwulan == "Q3 2025" | triwulan == "Q4 2025") %>%
  select(Uraian, triwulan, nilai) %>%
  mutate(kategori = rep("Q to Q", 8))

df_pertumbuhan_34 <- rbind(df_pertumbuhan_y_34, df_pertumbuhan_q_34)

df_pertumbuhan_34_wide <- df_pertumbuhan_34 %>%
  pivot_wider(
    id_cols = Uraian,
    names_from = c(triwulan, kategori),
    values_from = nilai
  ) %>%
  mutate(
    Uraian = if_else(Uraian == "PRODUK DOMESTIK REGIONAL BRUTO", "PDRB", Uraian)
  )

create_table_pertumbuhan <- function(df_wide = NULL) {
  df_wide %>%
    gt() %>%
    tab_spanner(label = "Y to Y", columns = contains("Y to Y")) %>%
    tab_spanner(label = "Q to Q", columns = contains("Q to Q")) %>%
    cols_label(
      `Q3 2025_Y to Y` = "Triwulan III 2025",
      `Q4 2025_Y to Y` = "Triwulan IV 2025",
      `Q3 2025_Q to Q` = "Triwulan III 2025",
      `Q4 2025_Q to Q` = "Triwulan IV 2025",
    ) %>%
    cols_label(Uraian = 'Komponen') %>%
    fmt_number(dec_mark = ",", sep_mark = ".", decimals = 2) %>%
    tab_footnote(
      footnote = "y-on-y: PDRB ADHK pada suatu triwulan dibandingkan dengan triwulan yang sama tahun sebelumnya",
      locations = cells_column_spanners(spanners = contains("Y to Y"))
    ) %>%
    tab_footnote(
      footnote = "q-to-q: PDRB ADHK pada suatu triwulan dibandingkan dengan triwulan sebelumnya",
      locations = cells_column_spanners(spanners = contains("Q to Q"))
    ) %>%
    data_color(
      colors = c("#AA5F81", "#f1c161"),
      columns = !contains("Uraian")
    ) %>%
    data_color(
      colors = colors[2],
      columns = contains("Uraian")
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_labels()
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_spanners()
    ) %>%
    tab_options(
      table.border.top.color = "white",
      table.border.right.color = "white",
      table.border.bottom.color = "white",
      table.border.left.color = "white",

      table_body.hlines.color = "white",
      table_body.vlines.color = "white",
      heading.border.bottom.color = "white",

      table.border.top.width = px(1),
      table.border.bottom.width = px(1),
      table.font.size = "10pt",

      data_row.padding = px(20),
      column_labels.padding = px(25),
    )
}

create_table_distribusi <- function(df = NULL) {
  df %>%
    mutate(
      Uraian = if_else(
        Uraian == "PRODUK DOMESTIK REGIONAL BRUTO",
        "PDRB",
        Uraian
      )
    ) %>%
    gt() %>%
    cols_label(
      `Q1 2025` = "Triwulan I 2025",
      `Q2 2025` = "Triwulan II 2025",
      `Q3 2025` = "Triwulan III 2025",
      `Q4 2025` = "Triwulan IV 2025",
      Uraian = "Komponen"
    ) %>%
    fmt_number(
      dec_mark = ",",
      sep_mark = "."
    ) %>%
    data_color(
      colors = c("#AA5F81", "#f1c161"),
      columns = !contains("Uraian")
    ) %>%
    data_color(
      colors = colors[2],
      columns = contains("Uraian")
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_labels()
    ) %>%
    gt::tab_style(
      style = cell_fill(color = colors[2]),
      locations = cells_column_spanners()
    ) %>%
    tab_options(
      table.border.top.color = "white",
      table.border.right.color = "white",
      table.border.bottom.color = "white",
      table.border.left.color = "white",

      table_body.hlines.color = "white",
      table_body.vlines.color = "white",
      heading.border.bottom.color = "white",

      table.border.top.width = px(1),
      table.border.bottom.width = px(1),
      table.font.size = "10pt",

      data_row.padding = px(20),
      column_labels.padding = px(25),
    )
}
