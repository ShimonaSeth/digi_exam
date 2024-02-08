# Loading required packages
library(tidyverse)
library(purrr)
library(rio)
library(here)
library(janitor)
library(lubridate)
library(matchmaker)
library(epikit)
library(tidyverse)
library(forecast)
library(styler)

# Load the DT package for creating interactive tables
library(DT)

# Load the data
data <- read.csv("adverse_events.csv")

# Check the structure of the data
str(data)

# Remove commas and convert "Count", "Population", "Year" to numeric
data$Count <- as.numeric(gsub(",", "", data$Count))
data$Population <- as.numeric(gsub(",", "", data$Population))
data$Year <- as.numeric(gsub(",", "", data$Year))  

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values[missing_values > 0])

# View data summary
summary(data)
head(data)
glimpse(data)

# Create a table using the DT package
DT::datatable(data)

# Load necessary libraries for plotting
library(ggplot2)
library(RColorBrewer)

# Aggregate data by Year to get the total count
total_count_data <- data %>%
  group_by(Year) %>%
  summarise(total_count = sum(Count))

# Create a line plot for the total count over the years
ggplot(total_count_data, aes(x = Year, y = total_count)) +
  geom_line(color = "#0072B2", linewidth = 1.5) +
  geom_point(color = "#0072B2", size = 3) +
  labs(title = "Total Count Trend Over the Years", y = "Total Count", x = "Year") +
  scale_x_continuous(breaks = seq(min(total_count_data$Year), max(total_count_data$Year), by = 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  guides(linewidth = guide_legend(title = "Line Width"))

# Group by County, calculate the total count
county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(5, total_count)

# Use a darker shade of blue from the "Blues" palette
blue_palette <- colorRampPalette(rev(brewer.pal(9, "Blues")))(5)

# Create a bar plot with a range of blue shades
ggplot(county_summary, aes(x = reorder(County, -total_count), y = total_count, fill = as.factor(County))) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = blue_palette) +
  labs(title = "Top 5 Counties by Total Count of Adverse Events", y = "Total Count", x = "County") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_line(color = "lightgray", linetype = "dotted"),
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

# Filter data for the statewide county
statewide_data <- data %>%
  filter(County == "STATEWIDE")

# Group by Year and count the occurrences
statewide_counts <- aggregate(County ~ Year, data = statewide_data, length)

# Rename the columns for clarity
colnames(statewide_counts) <- c('Year', 'Event_Counts')

# Filter data for the years 2005-2015
AE_table_sum <- data %>%
  group_by(Year) %>%
  summarise(
    Sum_ObsRate = sum(ObsRate)
  ) %>%
  ungroup()

# Create a data frame
df_table <- data.frame(
  Year = AE_table_sum$Year,
  Sum_ObsRate = AE_table_sum$Sum_ObsRate
)

#Creating a plot grouped by PSIDescription
plot_ObsRate <- ggplot(data, aes(x = Year, y = ObsRate, fill = PSIDescription)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Observed Rate for Each PSIDescription (2005-2015)",
    x = "Year",
    y = "Observed Rate (per 100,000)",
    fill = "PSIDescription"
  ) +
  theme(legend.position = "right")

library(plotly)

# Convert the ggplot object to a plotly object
plotly_ObsRate <- ggplotly(plot_ObsRate, tooltip = c("PSIDescription", "ObsRate"))

# Print the interactive plot
print(plotly_ObsRate)

# Group by Year and calculate the average ObsRate for Statewide
agg_data <- data %>%
  filter(County == "STATEWIDE") %>%
  group_by(Year) %>%
  summarise(avg_ObsRate = mean(ObsRate, na.rm = TRUE))

# Convert Year to integer without decimals
agg_data$Year <- as.integer(round(agg_data$Year))

# Create a line plot for the average ObsRate over the years
ggplot(agg_data, aes(x = Year, y = avg_ObsRate)) +
  geom_line(color = "#0072B2", size = 1.5) +
  geom_point(color = "#0072B2", size = 3) +
  labs(title = "Adverse Events Trend in Statewide",
       x = "Year",
       y = "Counts per 100,000 population") +
  scale_x_continuous(breaks = seq(min(agg_data$Year), max(agg_data$Year), by = 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

# Calculate the total ObsRate for each PSIDescription
psi_summary <- data %>%
  group_by(PSIDescription) %>%
  summarise(total_obs_rate = sum(ObsRate)) %>%
  arrange(desc(total_obs_rate))

# Calculate the percentage of ObsRate for each PSIDescription
psi_summary$percentage <- psi_summary$total_obs_rate / sum(psi_summary$total_obs_rate) * 100

# Create a data frame for plotly
pie_data <- data.frame(
  Category = psi_summary$PSIDescription,
  Value = psi_summary$percentage
)

# Define a custom color palette with colorful shades
color_palette <- c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9", "#92A8D1", "#955251", "#B565A7", "#009B77", "#DD4124", "#D65076")

plot_pie <- plot_ly(pie_data, labels = ~Category, values = ~Value, type = "pie", marker = list(colors = color_palette)) %>%
  layout(title = "Distribution of Categories",
         font = list(size = 14, family = "Arial"),
         hoverlabel = list(bgcolor = "white", font = list(size = 14)),
         legend = list(orientation = "h", x = 0.5, y = -0.1))

# Show the interactive pie chart
plot_pie

# Filter data for Lake County
lake_county_data <- data %>%
  filter(County == "Lake")

# Filter data for Statewide
statewide_data <- data %>%
  filter(County == "STATEWIDE")

# Group by Year and calculate the average ObsRate for Lake County
avg_obsrate_lake_county <- lake_county_data %>%
  group_by(Year) %>%
  summarise(avg_ObsRate = mean(ObsRate, na.rm = TRUE)) %>%
  mutate(County = "Lake")  # Adding the county name

# Group by Year and calculate the average ObsRate for Statewide
avg_obsrate_statewide <- statewide_data %>%
  group_by(Year) %>%
  summarise(avg_ObsRate = mean(ObsRate, na.rm = TRUE)) %>%
  mutate(County = "STATEWIDE")  # Adding the county name

# Combine the dataframes
combined_data <- bind_rows(avg_obsrate_lake_county, avg_obsrate_statewide)

# Convert Year to numeric
combined_data$Year <- as.numeric(combined_data$Year)

# Create comparison plot for both Lake County and Statewide ObsRate over the years
plot_comparison <- ggplot(combined_data, aes(x = Year, y = avg_ObsRate, color = County)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Comparison of Observed Rates: Lake County vs. Statewide", y = "Adverse Event Observed Rate (per 100,000)", x = "Year") +
  scale_color_manual(values = c("Lake" = "#0072B2", "STATEWIDE" = "#FF5733"), name = "County") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(min(combined_data$Year), max(combined_data$Year), by = 1))

# Show the comparison plot
plot_comparison
