# UBER NIGHTLIFE ANALYSIS 
# --------------------------------------------------
# PURPOSE: 
# Analyzes Uber pickup patterns during nightlife hours (10PM-3AM) in NYC (April 2014)
# Generates 4 key outputs:
# 1. hourly_pickups.png       - Bar chart of hourly demand
# 2. pickup_heatmap.html      - Interactive heatmap with legend
# 3. hotspot_summary.csv      - Top 10 pickup locations with stats
# 4. daily_demand_trends.png  - Daily trends throughout April

# Load required packages
library(tidyverse)   # Data manipulation and visualization
library(lubridate)   # Date/time handling
library(leaflet)     # Interactive maps
library(leaflet.extras) # Heatmap functionality

# ----------------------------------
# 1. DATA LOADING AND CLEANING
# ----------------------------------
tryCatch({
  # Read raw Uber data from CSV file
  uber_data <- read_csv("uber-raw-data-apr14.csv",
                        col_types = cols(
                          `Date/Time` = col_character(),  # Read as character for parsing
                          Lat = col_double(),             # Latitude coordinate
                          Lon = col_double(),             # Longitude coordinate
                          Base = col_character()          # Uber base identifier
                        )) %>%
    # Parse and create new variables
    mutate(
      DateTime = parse_date_time(`Date/Time`, "mdy HMS"), # Convert to datetime format
      Date = as_date(DateTime),          # Extract date component
      Hour = hour(DateTime),             # Extract hour (0-23)
      # Classify days as Weekend (Sat/Sun) or Weekday
      DayType = ifelse(wday(DateTime, week_start = 1) %in% 6:7, "Weekend", "Weekday"),
      # Create human-readable time labels
      TimeLabel = case_when(
        Hour == 22 ~ "10PM",
        Hour == 23 ~ "11PM",
        Hour == 0 ~ "12AM",
        Hour == 1 ~ "1AM",
        Hour == 2 ~ "2AM",
        Hour == 3 ~ "3AM"
      )
    ) %>%
    # Filter for nightlife hours only (10PM-3AM inclusive)
    filter(Hour %in% c(22, 23, 0, 1, 2, 3)) %>%
    # Remove coordinates outside NYC area
    filter(between(Lat, 40.5, 40.9), between(Lon, -74.05, -73.7))
  
  # Success message with record count
  message("Data loaded successfully: ", nrow(uber_data), " records")
}, error = function(e) {
  # Error handling if data loading fails
  stop("DATA LOADING FAILED: ", e$message)
})

# ----------------------------------
# 2. HOURLY TRENDS VISUALIZATION
# ----------------------------------
# Create bar chart of pickups by hour and day type
hourly_plot <- uber_data %>%
  # Count pickups for each time label and day type
  count(TimeLabel, DayType) %>%
  # Create ggplot visualization
  ggplot(aes(factor(TimeLabel, levels = c("10PM","11PM","12AM","1AM","2AM","3AM")), 
             n, fill = DayType)) +
  geom_col(position = "dodge") +  # Side-by-side bars for weekend/weekday
  # Custom color scheme
  scale_fill_manual(values = c("Weekend" = "#FF6B6B", "Weekday" = "#4ECDC4")) +
  # Chart labels and titles
  labs(
    title = "Uber Nighttime Pickups (April 2014)",
    x = "Hour of Night",
    y = "Number of Pickups",
    fill = "Day Type"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "top")  # Legend on top

# Save plot as high-quality PNG
ggsave("hourly_pickups.png", hourly_plot, width = 8, height = 5, dpi = 300)

# ----------------------------------
# 3. INTERACTIVE HEATMAP WITH LEGEND
# ----------------------------------
# Prepare data for heatmap by counting pickups at each location
heatmap_data <- uber_data %>%
  count(Lat, Lon, name = "Pickup_Count")

# Create interactive Leaflet heatmap
heatmap <- heatmap_data %>%
  leaflet() %>%  # Initialize map
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  # Dark base map
  addHeatmap(
    lng = ~Lon, lat = ~Lat,         # Coordinate columns
    intensity = ~Pickup_Count,      # Use pickup count for intensity
    radius = 12,                    # Size of each heat point
    blur = 20,                      # Smoothing factor
    max = 0.05,                     # Maximum intensity threshold
    gradient = c("blue", "green", "yellow", "red")  # Color progression
  ) %>%
  # Add legend explaining color scale
  addLegend(
    position = "bottomright",
    colors = c("blue", "green", "yellow", "red"),
    labels = c("Low", "Medium", "High", "Very High"),
    title = "Pickup Density"
  )

# Save as self-contained HTML file
htmlwidgets::saveWidget(heatmap, "pickup_heatmap.html", selfcontained = TRUE)

# ----------------------------------
# 4. HOTSPOT IDENTIFICATION
# ----------------------------------
# Identify top 10 pickup locations by rounding coordinates
hotspot_summary <- uber_data %>%
  # Group by rounded coordinates (3 decimal places ~ 100m precision)
  group_by(Rounded_Lat = round(Lat, 3), Rounded_Lon = round(Lon, 3)) %>%
  # Calculate summary statistics for each location
  summarize(
    Total_Pickups = n(),               # Total number of pickups
    Avg_Hour = mean(Hour),             # Average pickup hour
    Weekend_Pct = round(mean(DayType == "Weekend"), 2),  # % weekend pickups
    .groups = "drop"
  ) %>%
  # Sort by pickup count (descending)
  arrange(desc(Total_Pickups)) %>%
  # Keep top 10 locations
  slice_head(n = 10) %>%
  # Add descriptive columns
  mutate(
    # Classify peak period based on average hour
    Peak_Period = case_when(
      Avg_Hour < 23 ~ "Early Night (10PM-11PM)",
      Avg_Hour >= 23 ~ "Late Night (12AM-3AM)"
    ),
    # Create general location descriptors
    Location_Type = paste(
      ifelse(Rounded_Lat > 40.75, "Northern", "Southern"),
      ifelse(Rounded_Lon < -73.98, "Western", "Eastern"),
      "Zone"
    )
  ) %>%
  # Select and rename final columns
  select(Location_Type, Lat = Rounded_Lat, Lon = Rounded_Lon, 
         Total_Pickups, Peak_Period, Weekend_Pct)

# Save results to CSV
write_csv(hotspot_summary, "hotspot_summary.csv")

# ----------------------------------
# 5. DAILY DEMAND TRENDS
# ----------------------------------
# Create line chart of daily pickup trends
daily_plot <- uber_data %>%
  # Count pickups by date and day type
  count(Date, DayType) %>%
  # Create ggplot visualization
  ggplot(aes(Date, n, color = DayType)) +
  geom_line(linewidth = 0.8) +       # Line plot
  geom_point(size = 1.5) +           # Points for each day
  # Custom color scheme
  scale_color_manual(values = c("Weekend" = "#E69F00", "Weekday" = "#56B4E9")) +
  # Chart labels and titles
  labs(
    title = "Daily Uber Pickups in April 2014",
    subtitle = "Nighttime hours (10PM-3AM)",
    x = "Date",
    y = "Total Pickups",
    color = "Day Type"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "top")  # Legend on top

# Save plot as high-quality PNG
ggsave("daily_demand_trends.png", daily_plot, width = 10, height = 5, dpi = 300)
