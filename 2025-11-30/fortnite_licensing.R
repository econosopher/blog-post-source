# Fortnite Licensing Empire Visualization
# 538-style chart with Fortnite colors

library(tidyverse)
library(ggtext)

# Define Fortnite color palette
fortnite_purple <- "#9D4EDD"
fortnite_yellow <- "#F0B000"
gap_gray <- "#7A7A7A"
bg_color <- "#F0F0F0"
grid_color <- "#D0D0D0"
text_color <- "#333333"

# Create the dataset
# Status: 1 = In Fortnite, 0 = Not in Fortnite, 2 = Coming Soon
# OtherGame: Only surprising third-party crossovers
data <- tribble(
  ~IP, ~DifficultyScore, ~Status, ~OtherGame,
  # Difficulty 5 - White Whale
  "Nintendo", 5, 0, "Mario+Rabbids",
  "LVMH / Gucci", 5, 0, "LoL",
  "James Bond", 5, 2, NA,
  "Ferrari", 5, 1, "Gran Turismo",
  "Porsche", 5, 1, "Forza",

  # Difficulty 4 - Very Hard
  "Studio Ghibli", 4, 0, "Ni No Kuni",
  "Top Gun", 4, 0, "Ace Combat 7",
  "Harry Potter", 4, 2, NA,
  "Star Wars", 4, 1, "Soulcalibur IV",
  "Marvel", 4, 1, "Marvel vs. Capcom",
  "NFL", 4, 1, NA,
  "NBA", 4, 1, NA,

  # Difficulty 3 - Hard
  "Nike", 3, 1, "NBA 2K",
  "Hello Kitty / Sanrio", 3, 0, NA,

  # Difficulty 2 - Medium
  "Barbie", 2, 0, NA,
  "Transformers", 2, 1, NA,
  "TMNT", 2, 1, "Injustice 2",
  "John Wick", 2, 1, "Payday 2",
  "Rick & Morty", 2, 1, "Multiversus",

  # Difficulty 1 - Easy
  "Among Us", 1, 1, NA,
  "Nerf", 1, 1, NA,
  "Street Fighter", 1, 1, "Super Smash Bros"
)

# Sort data: by difficulty (desc), then by Status (desc), then alphabetically
data <- data %>%
  mutate(
    Status_factor = factor(
      Status,
      levels = c(1, 2, 0),
      labels = c("In Fortnite", "Coming Soon", "Not in Fortnite")
    )
  ) %>%
  arrange(desc(DifficultyScore), desc(Status), IP) %>%
  mutate(IP = factor(IP, levels = rev(IP)))

# Create the plot
p <- ggplot(data, aes(x = DifficultyScore, y = IP)) +
  # Subtle vertical grid lines at each difficulty level
  geom_vline(xintercept = 1:5, color = grid_color, linewidth = 0.4, linetype = "dashed") +

  # Points - Different shapes and colors for each status
  geom_point(
    aes(color = Status_factor, shape = Status_factor),
    size = 4.5,
    stroke = 1.2
  ) +

  # Add "Other Game" labels - more prominent
  geom_text(
    data = data %>% filter(!is.na(OtherGame)),
    aes(label = OtherGame),
    hjust = 0,
    nudge_x = 0.18,
    size = 3.2,
    color = "#555555",
    fontface = "bold.italic"
  ) +

  # Custom colors and shapes - circle, triangle, diamond
 scale_color_manual(
    values = c(
      "In Fortnite" = fortnite_purple,
      "Coming Soon" = fortnite_yellow,
      "Not in Fortnite" = gap_gray
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "In Fortnite" = 16,
      "Coming Soon" = 17,
      "Not in Fortnite" = 18
    ),
    name = NULL
  ) +

  # X-axis formatting
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Easy", "Medium", "Hard", "Very Hard", "White\nWhale"),
    limits = c(0.5, 6.2),
    expand = c(0, 0)
  ) +

  # Labels
  labs(
    title = "Fortnite's Licensing Empire",
    subtitle = "How Fortnite has secured some of gaming's hardest-to-get IP licenses",
    x = "Licensing Difficulty",
    y = "Intellectual Property",
    caption = "Notable third-party crossovers shown in bold italics"
  ) +

  # 538-style theme
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    # Background
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),

    # Grid
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_color, linewidth = 0.3),
    panel.grid.minor.y = element_blank(),

    # Axis
    axis.text.y = element_text(color = text_color, size = 10, hjust = 1),
    axis.text.x = element_text(color = text_color, size = 9, face = "bold"),
    axis.title.x = element_text(color = text_color, size = 11, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(color = text_color, size = 11, face = "bold", margin = margin(r = 12)),
    axis.ticks = element_blank(),

    # Title and subtitle
    plot.title = element_text(
      color = text_color,
      size = 20,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      color = "#555555",
      size = 11,
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      color = "#666666",
      size = 8,
      hjust = 0,
      margin = margin(t = 15)
    ),

    # Legend - positioned below subtitle
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 15),
    legend.text = element_text(size = 10, color = text_color),
    legend.key = element_rect(fill = bg_color, color = NA),
    legend.key.size = unit(0.8, "cm"),

    # Margins
    plot.margin = margin(20, 20, 20, 20)
  ) +

  # Guide adjustments
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    shape = guide_legend(override.aes = list(size = 5))
  )

# Save the plot - compact width for LinkedIn
ggsave(
  "fortnite_licensing.png",
  plot = p,
  width = 7,
  height = 7.2,
  dpi = 300,
  bg = bg_color
)

# Display
print(p)
