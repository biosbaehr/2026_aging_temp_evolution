# =========================
# 1. Load libraries
# =========================

rm(list = ls())

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# File path
file_path <- "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/2026-06-18_CLEAN_temps_shapley_ants_1920_1924.xlsx"

# =========================
# 2. Import data
# =========================

df <- read_excel(file_path)

df$Year <- as.factor(df$Year)

df$`Temp (°C)` <- as.numeric(df$`Temp (°C)`)
df$`Speed (cm/s)` <- as.numeric(df$`Speed (cm/s)`)
df$Year <- as.factor(df$Year)

str(df)

# =========================
# 3. Dot plot
# =========================
ggplot(df,
       aes(x = `Temp (°C)`,
           y = `Speed (cm/s)`,
           color = Year)) +
  
  geom_point(size = 2, alpha = 0.8) +
  
  geom_smooth(
    method = "glm",
    formula = y ~ x,
    method.args = list(family = gaussian(link = "log")),
    se = FALSE,
    linewidth = 1.2
  ) +
  # 1. Horizontal Bracket (ΔT = 10°C)
  # Main horizontal bar (y and yend must match!)
  annotate("segment", x = 20, y = 0.8, xend = 30, yend = 0.8, color = "black") +
  # Left tick pointing up (x and xend match, y goes up)
  annotate("segment", x = 20, y = 0.8, xend = 20, yend = 1.0, color = "black") +
  # Right tick pointing up (x and xend match, y goes up)
  annotate("segment", x = 30, y = 0.8, xend = 30, yend = 1.0, color = "black") +
  # Text label
  annotate("text", x = 25, y = 0.4, label = "ΔT = 10°C", size = 4) +
  
  # 2. Vertical Bracket (≈ 2-fold increase)
  # Main vertical bar (x and xend must match!)
  annotate("segment", x = 31, y = 1.0, xend = 31, yend = 2.6, color = "black") +
  # Bottom tick pointing left (y and yend match, xend steps to the left)
  annotate("segment", x = 31, y = 1.0, xend = 30.2, yend = 1.0, color = "black") +
  # Top tick pointing left (y and yend match, xend steps to the left)
  annotate("segment", x = 31, y = 2.6, xend = 30.2, yend = 2.6, color = "black") +
  # Multiline Text label
  annotate("text", x = 32.0, y = 1.8, label = "≈2-fold\nincrease\nin speed", size = 4, hjust = 0) +
  
  # ==========================================

scale_color_discrete(
  name = NULL,
  labels = c(
    expression("1920 " * italic("L. apiculatum")),
    expression("1924 " * italic("T. sessile"))
  )
) +
  
  scale_x_continuous(
    limits = c(10, 40),
    breaks = seq(10, 40, by = 5)
  ) +
  
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 7, by = 0.5)
  ) +
  
  labs(
    x = "Temperature (°C)",
    y = "Movement Speed (cm/s)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.3
    )
  ) +
  
  
  theme(
    
    legend.position = c(0.03, 0.98),
    legend.justification = c(0, 1),
    
    legend.background = element_blank(),
    legend.key = element_blank()
  )
  

