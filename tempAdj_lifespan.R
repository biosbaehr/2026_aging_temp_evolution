# Install required packages if you haven't already:
# install.packages("ggplot2")
# install.packages("readxl")

# Load required libraries
library(ggplot2)
library(readxl)

# 1. Load the data using your specific pathname
data <- read_excel("/Users/CooperTech/Downloads/tempAdj_lifespan.xlsx")

# 2. Preserve the original order of organisms as listed in the file
data$Organism <- factor(data$Organism, levels = unique(data$Organism))

# 3. Define human baseline parameters (from the 'H. Sapiens' row)
T_human <- 310.0
human_lifespan <- 122.0

# 4. Calculate the exact temperature-adjusted lifespan using the Arrhenius equation
data$Adjusted_Lifespan <- data$Lifespan * (exp(-48000 / (8.314 * data$Temperature)) / exp(-48000 / (8.314 * T_human)))

# 5. Create the visualization
ggplot(data, aes(x = Organism, y = Adjusted_Lifespan)) +
  # Green points instead of bars
  geom_point(color = "forestgreen", size = 4) +
  # Dark blue dotted line running across the whole graph at the human baseline
  geom_hline(yintercept = human_lifespan, linetype = "dotted", color = "darkblue", size = 1) +
  # Label the baseline with a matching dark blue color
  #annotate("text", x = 1.5, y = human_lifespan + 25, color = "darkblue", fontface = "bold") +
  # Log transform the y-axis (base 10) and explicitly define every single tick and its label
  # Adjusted to catch 10 on the low end and stop around 400 on the high end
  scale_y_log10(
    breaks = c(10, 20, 30, 50, 100, 200, 400, 800),
    labels = c("10", "20", "30", "50", "100", "200", "400", "800")
  ) +
  # Labels and styling (using expression() for the subscript)
  labs(
    title = "Temperature-Adjusted Lifespan",
    x = NULL,
    y = expression("Lifespan (years)")
  ) +
  theme_minimal() +
  theme(
    # Slanted, italicized names on the x-axis
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "italic", size = 11),
    # Explicitly makes the y-axis numbers 1 pt larger than the x-axis text
    axis.text.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
    panel.grid.major.x = element_blank(), # Cleans up the background vertical lines
    panel.grid.minor.y = element_blank()  # Forces ggplot to only draw the explicitly labeled grid lines
  )

