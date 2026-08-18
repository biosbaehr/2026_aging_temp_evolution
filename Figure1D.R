rm(list = ls())

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read in the data "C:\Users\baehr\OneDrive\Desktop\LynchLab\Temperature_Aging_2026\2026-06-29_dmel_survivorship_temps_digitized.xlsx"
data <- read_excel(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/2026-06-29_dmel_survivorship_temps_digitized.xlsx"
)

# Make temperature a factor in plotting order
data$Temp <- factor(
  data$Temp,
  levels = c("30°C", "27°C", "21°C", "18°C")
)

# Select a labeling point for each curve
# (last point where survivorship is still above 20%)
label_data <- data %>%
  group_by(Temp) %>%
  filter(Survivorship > 0.2) %>%
  slice_tail(n = 1)

# Create survivorship plot
p <- ggplot(
  data,
  aes(
    x = `Age (days)`,
    y = Survivorship,
    color = Temp,
    group = Temp
  )
) +
  
  # Connect points with lines
  geom_line(linewidth = 1) +
  
  # Plot points
  geom_point(size = 2) +
  
  # Add labels beside the curves
  geom_text(
    data = label_data,
    aes(label = Temp),
    hjust = -0.25,
    vjust = -0.3,
    size = 4,
    show.legend = FALSE
  ) +
  
  # Shades of green
  scale_color_manual(
    values = c(
      "18°C" = "#00441B",  # darkest green
      "21°C" = "#3f6e22",  # dark green
      "27°C" = "#4D9221",  # medium green
      "30°C" = "#a8dc4d"   # light green
    )
  ) +
  
  # Labels
  labs(
    x = "Age (days)",
    y = "Survivorship"
  ) +
  
  # Axis formatting
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  
  scale_x_continuous(
    expand = c(0.02, 0)
  ) +
  
  # Theme
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = "black")
  )

# Display plot
print(p)

# Save figure
ggsave(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/dmel_survivorship_curve.pdf",
  p,
  width = 6,
  height = 5
)

