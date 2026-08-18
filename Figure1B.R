
######
####
#####
####

# Clear the environment
rm(list = ls())

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. File path
file_path <- "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/2026-07-21_Tetrahymena_digitized_Y_to_R_calibrated_WITH_ECOLI.xlsx"

# 2. Read the data
df <- read_excel(file_path)

# Clean column names
colnames(df) <- c("Temperature_C", "X_1000_T", "Digitized_Y", 
                  "Estimated_R_div_1", "Estimated_R_div_2", 
                  "Estimated_generation", "Organism", "ln_k")

# 3. Clean, filter, shorten organism label, and round Temperature
plot_data <- df %>%
  filter(!is.na(X_1000_T), !is.na(ln_k)) %>%
  mutate(
    # Shorten T. pyriformis label
    Organism = ifelse(grepl("pyriformis|tetra", Organism, ignore.case = TRUE), "T. pyr", Organism),
    # Round Temperature_C to the nearest 0.5 degrees
    Temp_Rounded = round(Temperature_C * 2) / 2
  )

# Subsample every other temperature per Organism group for annotation
label_data <- plot_data %>%
  group_by(Organism) %>%
  arrange(X_1000_T) %>%
  slice(seq(1, n(), by = 2)) %>%
  ungroup()

# 4. Subset datasets for linear fits
ecoli_fit_data <- plot_data %>%
  filter(
    grepl("coli", Organism, ignore.case = TRUE),
    Temperature_C >= 20 & Temperature_C <= 37
  )

t_pyr_fit_data <- plot_data %>%
  filter(
    grepl("pyr|tetra", Organism, ignore.case = TRUE),
    Temperature_C >= 17 & Temperature_C <= 27.6
  )

# 5. Fit Linear Models to calculate Slope and R^2 values (STORED IN R ENVIRONMENT)
ecoli_lm <- lm(ln_k ~ X_1000_T, data = ecoli_fit_data)
t_pyr_lm <- lm(ln_k ~ X_1000_T, data = t_pyr_fit_data)

# Extracted metrics saved in workspace variables
ecoli_slope <- round(coef(ecoli_lm)[2], 3)
ecoli_r2    <- round(summary(ecoli_lm)$r.squared, 3)

t_pyr_slope <- round(coef(t_pyr_lm)[2], 3)
t_pyr_r2    <- round(summary(t_pyr_lm)$r.squared, 3)

# Print stored model metrics to the R console
cat("--- E. coli Fit (20–37°C) ---\n",
    "Slope:", ecoli_slope, "| R²:", ecoli_r2, "\n\n",
    "--- T. pyr Fit (17–27.6°C) ---\n",
    "Slope:", t_pyr_slope, "| R²:", t_pyr_r2, "\n")

# 6. Create the Arrhenius Plot
ggplot(plot_data, aes(x = X_1000_T, y = ln_k, color = Organism)) +
  
  # Raw experimental data points
  geom_point(size = 2.5) +
  
  # Best-fit dashed lines (extrapolated full range)
  geom_smooth(
    data = ecoli_fit_data,
    method = "lm",
    se = FALSE,
    linetype = "solid",
    linewidth = .9,
    fullrange = TRUE
  ) +
  geom_smooth(
    data = t_pyr_fit_data,
    method = "lm",
    se = FALSE,
    linetype = "solid",
    linewidth = .9,
    fullrange = TRUE
  ) +
  
  # Annotate ONLY every other temperature point
  geom_text_repel(
    data = label_data,
    aes(label = paste0(Temp_Rounded, "°C")),
    size = 3.2,
    show.legend = FALSE,
    box.padding = 0.35,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  # Color palette matching theme
  scale_color_manual(values = c("#E7298A", "#7570B3")) +
  
  # Axis labels using math notation
  labs(
    x = expression(1000 / T ~ (K)),
    y = expression(ln(k) ~ "(cell div / day)"),
    color = NULL
  ) +
  
  # Black and White theme with custom gridlines and borderless italic legend
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    
    # Legend styling and positioning
    legend.position = c(0.5, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    
    # Italicize legend labels
    legend.text = element_text(face = "italic")
  )

