# Load necessary libraries 
library(ggplot2) 
library(dplyr) 





seine_meta <- read.csv("/Users/marisgoodwin/Documents/GITHUB/AMSS_2025/inputs/environmental/JNKB_20212022_Point_Sampling.csv")
env <- seine_meta %>% 
  dplyr::select(Turbidity_mean, DissolvedOxygen, SpecificConductivity, Salinity, Temperature)

# Normalize the numeric columns
env$SamplingDate <- seine_meta$SamplingDate

# Add the metadata back to the normalized data
env <- env %>% 
  left_join(seine_meta, by = "SamplingDate")

# Take the averages of the env variables based on site_id, year, region and month
avg <- env %>% 
  group_by(region, site_id, SamplingDate, SamplingYear, SamplingMonth) %>% 
  summarise(mean_DO = mean(DissolvedOxygen.x, na.rm = TRUE),
            SpecificConductivity_mean = mean(SpecificConductivity.x,na.rm = TRUE),
            Salinity_mean = mean(Salinity.x,na.rm = TRUE),
            Temperature_mean = mean(Temperature.x,na.rm = T)) %>%   # Include the mean turbidity
  ungroup()

# Plot the averages
avg_long <- avg %>%
  dplyr::select(SamplingDate, mean_DO, SpecificConductivity_mean, Salinity_mean, Temperature_mean, SamplingYear, SamplingMonth, site_id, region) %>%
  gather(key = "Variable", value = "Value", -SamplingDate)
avg_long <- avg_long %>% 
  left_join(avg, by = "SamplingDate", relationship = "many-to-many")

avg_long$SamplingMonth <- factor(avg_long$SamplingMonth, levels = c("April", "May", "June", "July", "August", "September", "October"))

# Define the desired order of site_id levels
site_order <- c("jnmri", "jneri", "jncci", "jnsci", 
                "kbgwi", "kbwsi", "kbhbi", "kbtui", "kbjki")

# Convert site_id to a factor with the specified order
avg_long$site_id <- factor(avg_long$site_id, levels = site_order)

# Verify the levels
levels(avg_long$site_id)


# Define the colors for each site_id

site_colors <- c(
  "jnmri" = "#045a8d",
  "jneri" = "#2b8cbe",
  "jncci" = "#bdc9e1",
  "jnsci" = "#f1eef6",
  "kbgwi" = "#045a8d",
  "kbwsi" = "#2b8cbe",
  "kbhbi" = "#74a9cf",
  "kbtui" = "#bdc9e1",
  "kbjki" = "#f1eef6"
)
#f1eef6
#bdc9e1
#74a9cf
#2b8cbe
#045a8d


ggplot(avg_long, aes(x = SamplingMonth, y = Salinity_mean, color = site_id)) +
  geom_line(aes(group = site_id, color = site_id), size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ region + SamplingYear) +
  theme(panel.background = element_rect(fill = "grey20"),
  panel.grid.minor = element_blank())
ggsave("Salinity_plot.pdf")



ggplot(avg_long, aes(x = SamplingMonth, y = Temperature_mean,color=site_id)) +
  geom_line(aes(group = site_id, color = site_id), size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ region + SamplingYear) +
  theme(panel.background = element_rect(fill = "grey20"),
        panel.grid.minor = element_blank())

ggsave("Temperature_plot.pdf")

ggplot(avg_long, aes(x = SamplingMonth, y = SpecificConductivity_mean,color=site_id)) +
  geom_line(aes(group = site_id, color = site_id), size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ region + SamplingYear) +
  theme(panel.background = element_rect(fill = "grey20"),
        panel.grid.minor = element_blank())
ggsave("SpecificConductivity_plot.pdf")

ggplot(avg_long, aes(x = SamplingMonth, y = mean_DO,color=site_id)) +
  geom_line(aes(group = site_id, color = site_id), size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ region + SamplingYear) +
  theme(panel.background = element_rect(fill = "grey20"),
        panel.grid.minor = element_blank())

ggsave("DissolvedOxygen_plot.pdf")

ggplot(avg_long, aes(x = SamplingMonth, y = mean_DO, fill = site_id)) +
       geom_area(aes(group = site_id)) +  # Grouping by site_id
       scale_fill_manual(values = site_colors) +  # Use scale_fill_manual for area colors
       facet_wrap(~ region + SamplingYear) +
       theme(
             panel.background = element_rect(fill = "grey20"),
             panel.grid.minor = element_blank()
         ) +
       labs(title = "Stacked Area Plot of mean_DO over Time", x = "Sampling Month", y = "Dissolved Oxygen (mean_DO)")

#join the turbidity mean to avg_long from the seine_meta 
avg_long <- avg_long %>% 
  left_join(seine_meta, by = c("SamplingDate","site_id"),relationship = "many-to-many")
# scale the turbidity data 
avg_long$Turbidity_mean <- scale(avg_long$Turbidity_mean)

ggplot(avg_long, aes(x = SamplingMonth.x, y = Turbidity_mean,color=site_id)) +
  geom_line(aes(group = site_id,color=site_id)) +
  geom_point() +
  facet_wrap(~ region.x + SamplingYear.x)

ggsave("DissolvedOxygen_plot.png")


# Plot the normalized data
seine_meta_long <- seine_meta %>%
  select(SamplingDate, DissolvedOxygen, SpecificConductivity, Salinity, Temperature) %>%
  gather(key = "Variable", value = "Value", -SamplingDate)

ggplot(seine_meta_long, aes(x = SamplingDate, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Normalized Water Quality Data", x = "Sampling Date", y = "Normalized Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








