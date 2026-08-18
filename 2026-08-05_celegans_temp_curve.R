
rm(list = ls())

# Load libraries
library(readxl)
library(ggplot2)

# Read in the data
data <- read_excel(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/2026-06-29_celegans_intro_survivorship_temps.xlsx"
)

# Make Temperature a factor and specify plotting order
data$Temperature <- factor(
  data$Temperature,
  levels = c("25°C", "20°C", "15°C")
)

# Create the plot
p <- ggplot(data,
            aes(x = `Age (days)`,
                y = Survivorship,
                color = Temperature,
                group = Temperature)) +
  
  # Add lines
  geom_line(linewidth = 1.2) +
  
  # Add points
  geom_point(size = 3) +
  
  # Custom blue palette
  scale_color_manual(
    values = c(
      "25°C" = "#9ECAE1",   # light blue  
      "20°C" = "#4292C6",  # medium blue
      "15°C" = "#08306B"   # dark blue
    )
  ) +
  
  # Axis labels
  labs(
    x = "Age (days)",
    y = "Survivorship",
    color = "Temp"
  ) +
  
  # Set axis limits
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
    legend.position = c(0.85, 0.80),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

# Display plot
print(p)

# Save figure
ggsave(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/celegans_survivorship_curve.pdf",
  p,
  width = 6,
  height = 5
)


####
#
####
#####

rm(list = ls())

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read in the data
data <- read_excel(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/2026-06-29_celegans_intro_survivorship_temps.xlsx"
)

# Set plotting order
data$Temperature <- factor(
  data$Temperature,
  levels = c("25°C", "20°C", "15°C")
)

# Choose a labeling point for each curve
label_data <- data %>%
  group_by(Temperature) %>%
  filter(Survivorship > 0.20) %>%
  slice_tail(n = 1)

# Create plot
p <- ggplot(
  data,
  aes(
    x = `Age (days)`,
    y = Survivorship,
    color = Temperature,
    group = Temperature
  )
) +
  
  # Lines
  geom_line(linewidth = 1.0) +
  
  # Points
  geom_point(size = 2) +
  
  # Labels attached to curves
  geom_text(
    data = label_data,
    aes(label = Temperature),
    hjust = -0.25,
    vjust = -0.30,
    size = 4,
    show.legend = FALSE
  ) +
  
  # Blue palette
  scale_color_manual(
    values = c(
      "25°C" = "#9ECAE1",
      "20°C" = "#4292C6",
      "15°C" = "#08306B"
    )
  ) +
  
  # Labels
  labs(
    x = "Age (days)",
    y = "Survivorship"
  ) +
  
  # Axes
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
    #panel.grid.minor = element_blank(),
    #panel.grid.major = element_line(
    #  color = "grey90",
    #  linewidth = 0.3
    #),
    panel.grid = element_blank(),
    legend.position = "none",
    #axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# Display plot
print(p)

# Save figure
ggsave(
  "C:/Users/baehr/OneDrive/Desktop/LynchLab/Temperature_Aging_2026/celegans_survivorship_curve.pdf",
  p,
  width = 6,
  height = 5
)

